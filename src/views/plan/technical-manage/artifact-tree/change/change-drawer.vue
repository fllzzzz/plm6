<template>
  <common-drawer
    ref="drawerRef"
    title="清单变更"
    v-model="drawerVisible"
    direction="rtl"
    :showClose="false"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleRight>
      <common-button @click="handleClose" size="mini" plain>取消本次变更</common-button>
    </template>
    <template #content>
      <el-card class="step-content">
        <common-step v-model="step" :options="stepOptions" space="33%" finish-status="success" />
        <span class="step-btn">
          <common-button size="mini" plain :disabled="step === 0" @click="step--">上一步</common-button>
          <common-button size="mini" plain :disabled="step === stepOptions.length - 1 || true" @click="step++">下一步</common-button>
          <common-button size="mini" type="warning" :disabled="step !== stepOptions.length - 1">确认提交</common-button>
        </span>
      </el-card>
      <component :is="currentView" :height-style="heightStyle" />
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, computed, ref, reactive, provide, watchEffect } from 'vue'

import { arr2obj, obj2arr } from '@/utils/convert/type'
import { toPrecision, isNotBlank, isBlank } from '@/utils/data-type'
import { changeTypeEnum, artifactHandleStatusEnum, assembleOperateTypeEnum } from './components/common.js'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import commonStep from '@comp-common/common-step/index'
import mHandle from './module/handle'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  originChangeInfo: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

// 高度
const { maxHeight, heightStyle } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.step-content'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    extraHeight: 50
  },
  drawerVisible
)

const stepOptions = reactive([{ title: '变更处理' }, { title: '变更汇总' }, { title: '技术成果上传' }, { title: '变更原因填写' }])
const stepComponent = [mHandle]
const step = ref(0)
const changeInfo = ref([])
const changeInfoMap = ref(new Map())
provide('changeInfo', changeInfo)
provide('changeInfoMap', changeInfoMap)
provide('originChangeInfo', props.originChangeInfo)

const currentView = computed(() => stepComponent[step.value])

function showHook() {
  const _list = []
  for (let i = 0; i < props.originChangeInfo.length; i++) {
    const v = props.originChangeInfo[i]
    v.partCompareResList = handleComparePartList(v.oldArtifact.partList || [], v.newArtifact.partList || [])
    v.assembleInfo = handleAssembleList(v.oldArtifact.assembleList || [], v.newArtifact.assembleList || [])
    v.artifactHandleStatus = getArtifactInitStatus(v)
    v.boolTag = false
    v.boolDel = false
    artifactWatch(v)
    _list.push(v)
    changeInfoMap.value.set(v.newArtifact.serialNumber, v)
  }
  console.log({ _list })
  changeInfo.value = _list
}

function artifactWatch(item) {
  watchEffect(() => {
    let isAssembleHandled = true
    const assembleChangeList = []
    const oldHandledSNs = []
    const assembleDelList = []
    for (let i = 0; i < item.assembleInfo.needHandleNewList.length; i++) {
      const v = item.assembleInfo.needHandleNewList[i]
      if (!v.operateType || (v.operateType !== assembleOperateTypeEnum.NEW.V && !v.oldSerialNumbers?.length)) isAssembleHandled = false

      if (v.operateType === assembleOperateTypeEnum.NEW.V) {
        const changeType = changeTypeEnum.NEW.V
        const diffQuantity = -v.quantity
        const diffTotalWeight = -v.totalNetWeight
        assembleChangeList.push({ newSN: v.serialNumber, oldSN: null, changeType, diffQuantity, diffTotalWeight })
      }

      if (v.operateType !== assembleOperateTypeEnum.NEW.V && v.oldSerialNumbers?.length) {
        for (let o = 0; o < v.oldSerialNumbers.length; o++) {
          const oldSN = v.oldSerialNumbers[o]
          oldHandledSNs.push(oldSN)
          if (!v.handleObj[oldSN] || !v.handleObj[oldSN]?.handleType || !v.handleObj[oldSN]?.quantity) {
            isAssembleHandled = false
            continue
          }

          const changeType = changeTypeEnum.EDIT.V
          const diffQuantity = v.handleObj[oldSN]?.quantity - v.quantity
          const diffTotalWeight = toPrecision(v.handleObj[oldSN]?.quantity * v.handleObj[oldSN]?.netWeight - v.totalNetWeight, 2)
          assembleChangeList.push({
            newSN: v.serialNumber,
            oldSN: oldSN,
            oldQuantity: v.handleObj[oldSN]?.quantity,
            handleType: v.handleObj[oldSN]?.handleType,
            changeType,
            diffQuantity,
            diffTotalWeight
          })
        }
      }
    }
    if (isAssembleHandled && oldHandledSNs.length !== item.assembleInfo.needHandleOldList.length) {
      for (let i = 0; i < item.assembleInfo.needHandleOldList.length; i++) {
        const o = item.assembleInfo.needHandleOldList[i]
        if (!oldHandledSNs.includes(o.serialNumber)) {
          const changeType = changeTypeEnum.DEL.V
          const diffQuantity = o.quantity
          const diffTotalWeight = o.totalNetWeight
          assembleDelList.push({ newSN: null, oldSN: o.serialNumber, changeType, diffQuantity, diffTotalWeight })
        }
      }
    }
    item.assembleCompareList = [...assembleChangeList, ...item.assembleInfo?.amList, ...assembleDelList]
    if (isAssembleHandled) {
      item.artifactHandleStatus = artifactHandleStatusEnum.HANDLED.V
    } else if (item.artifactHandleStatus === artifactHandleStatusEnum.HANDLED.V) {
      item.artifactHandleStatus = artifactHandleStatusEnum.UN_HANDLE.V
    }
    if (item.boolDel) {
      item.artifactHandleStatus = artifactHandleStatusEnum.CANCEL_HANDLE.V
    }
    if (item.boolTag) {
      item.artifactHandleStatus |= artifactHandleStatusEnum.TAG.V
    }
  })
}

// 获取构件初始状态
function getArtifactInitStatus(item) {
  let artifactHandleStatus
  if (
    isBlank(item.assembleInfo?.needHandleOldList) &&
    isBlank(item.assembleInfo?.needHandleNewList) &&
    isBlank(item.assembleInfo?.amList)
  ) {
    artifactHandleStatus = artifactHandleStatusEnum.NOT_HANDLE.V
  } else {
    artifactHandleStatus = artifactHandleStatusEnum.UN_HANDLE.V
  }
  return artifactHandleStatus
}

// 处理部件列表
function handleAssembleList(oldList, newList) {
  // 需要处理的列表，过滤完全相同的
  const needHandleOldList = []
  const needHandleOldObj = {}
  const needHandleNewList = []
  const needHandleNewObj = {}
  const amList = [] // 单纯加减数量的列表
  const sameKeys = [] // 编号、规格、长度相同的列表
  const oldObj = arr2obj(oldList, 'serialNumber')

  for (let i = 0; i < newList.length; i++) {
    const n = newList[i]
    const _o = oldObj[n.serialNumber]
    if (isNotBlank(_o) && _o.specification === n.specification && _o.length === n.length) {
      sameKeys.push(`${n.serialNumber}_${n.specification}_${n.length}`)
      if (_o.quantity === n.quantity) {
        continue
      } else {
        const diffQuantity = _o.quantity - n.quantity
        const changeType = n.diffQuantity > 0 ? changeTypeEnum.REDUCE.V : changeTypeEnum.ADD.V
        const diffTotalWeight = toPrecision(_o.totalNetWeight - n.totalNetWeight, 2)
        amList.push({ newSN: n.serialNumber, oldSN: _o.serialNumber, changeType, diffQuantity, diffTotalWeight })
      }
    } else {
      needHandleNewList.push(n)
      needHandleNewObj[n.serialNumber] = n
    }
  }

  for (let i = 0; i < oldList.length; i++) {
    const o = oldList[i]
    const _key = `${o.serialNumber}_${o.specification}_${o.length}`
    if (sameKeys.includes(_key)) {
      continue
    } else {
      needHandleOldList.push(o)
      needHandleOldObj[o.serialNumber] = o
    }
  }

  return {
    needHandleOldList,
    needHandleNewList,
    needHandleOldObj,
    needHandleNewObj,
    amList
  }
}

// 处理比较零件变更
function handleComparePartList(oldList, newList) {
  const resObj = {}
  const oldObj = arr2obj(oldList, 'serialNumber')
  const newObj = {}
  for (let i = 0; i < newList.length; i++) {
    const n = newList[i]
    const _o = oldObj[n.serialNumber]
    newObj[n.serialNumber] = n
    const newQuantity = n.quantity
    let changeType, diffQuantity, diffTotalWeight, oldQuantity
    // 新增
    if (!_o) {
      changeType = changeTypeEnum.NEW.V
      oldQuantity = 0
      diffQuantity = -newQuantity
      diffTotalWeight = -n.totalNetWeight
    } else if (_o.quantity === n.quantity) {
      continue
    } else {
      oldQuantity = _o.quantity
      diffQuantity = _o.quantity - newQuantity
      diffTotalWeight = toPrecision(_o.totalNetWeight - n.totalNetWeight, 2)
      changeType = n.diffQuantity > 0 ? changeTypeEnum.REDUCE.V : changeTypeEnum.ADD.V
    }
    resObj[n.serialNumber] = { ...n, changeType, diffQuantity, diffTotalWeight, oldQuantity, newQuantity }
  }
  for (let i = 0; i < oldList.length; i++) {
    const o = oldList[i]
    const oldQuantity = o.quantity
    let changeType, diffQuantity, diffTotalWeight, newQuantity
    if (!newObj[o.serialNumber]) {
      changeType = changeTypeEnum.DEL.V
      diffQuantity = oldQuantity
      diffTotalWeight = o.totalNetWeight
      newQuantity = 0
    } else {
      continue
    }
    resObj[o.serialNumber] = { ...o, oldQuantity, changeType, diffQuantity, diffTotalWeight, newQuantity }
  }
  return obj2arr(resObj)
}
</script>

<style lang="scss" scoped>
.step-content {
  position: relative;
  margin-bottom: 10px;

  ::v-deep(.el-card__body) {
    padding: 15px;
  }

  .step-btn {
    position: absolute;
    top: 50%;
    right: 15px;
    transform: translateY(-50%);
  }
}
</style>
