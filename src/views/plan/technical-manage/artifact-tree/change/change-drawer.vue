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
          <common-button size="mini" plain :disabled="step === stepOptions.length - 1" @click="handleNextStep">下一步</common-button>
          <common-button :loading="submitLoading" size="mini" type="warning" :disabled="step !== stepOptions.length - 1" @click="submit">
            确认提交
          </common-button>
        </span>
      </el-card>
      <component :is="currentView" :height-style="heightStyle" />
    </template>
  </common-drawer>
</template>

<script setup>
import { changeList } from '@/api/plan/technical-manage/artifact-tree'
import { defineProps, defineEmits, computed, ref, reactive, provide, watchEffect } from 'vue'
import { ElMessage, ElNotification } from 'element-plus'

import { arr2obj, obj2arr } from '@/utils/convert/type'
import { toPrecision, isNotBlank, isBlank } from '@/utils/data-type'
import { changeTypeEnum, artifactHandleStatusEnum, assembleOperateTypeEnum } from './components/common.js'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import commonStep from '@comp-common/common-step/index'
import mHandle from './module/handle'
import mSummary from './module/summary'
import mTechnicalUpload from './module/technical-upload'
import mReason from './module/reason'

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
  },
  monomerId: {
    type: [Number, undefined],
    default: undefined
  },
  projectId: {
    type: [Number, undefined],
    default: undefined
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
const stepComponent = [mHandle, mSummary, mTechnicalUpload, mReason]
const step = ref(0)
const submitLoading = ref(false)
const form = ref({})
const changeInfo = ref([])
const changeInfoMap = ref(new Map())
const summaryInfo = ref({})
provide('form', form)
provide('changeInfo', changeInfo)
provide('changeInfoMap', changeInfoMap)
provide('originChangeInfo', props.originChangeInfo)
provide('summaryInfo', summaryInfo)

const currentView = computed(() => stepComponent[step.value])
const handledQuantity = computed(() => {
  let num = 0
  changeInfo.value.forEach((v) => {
    if (
      v.artifactHandleStatus &
      (artifactHandleStatusEnum.HANDLED.V | artifactHandleStatusEnum.NOT_HANDLE.V | artifactHandleStatusEnum.CANCEL_HANDLE.V)
    ) {
      num++
    }
  })
  return num
})
provide('handledQuantity', handledQuantity)

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
        assembleChangeList.push({ newAssemble: v, oldAssemble: null, changeType, diffQuantity, diffTotalWeight })
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
            newAssemble: v,
            oldAssemble: item.assembleInfo?.needHandleOldObj?.[oldSN],
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
          assembleDelList.push({ newAssemble: null, oldAssemble: o, changeType, diffQuantity, diffTotalWeight })
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
        amList.push({ newAssemble: n, oldAssemble: _o, changeType, diffQuantity, diffTotalWeight })
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

// 处理汇总数据
function handleSummaryData() {
  const artifactObj = {}
  const assembleObj = {}
  const partObj = {}
  for (const artifact of changeInfo.value) {
    const _old = artifact.oldArtifact
    const _new = artifact.newArtifact
    const _key = `${_old.serialNumber}_${_old.specification}_${_old.length}_${_old.netWeight}__${_new.serialNumber}_${_new.specification}_${_new.length}_${_new.netWeight}`
    const diffQuantity = _old.quantity - _new.quantity || 0
    const diffTotalWeight = toPrecision(_old.totalNetWeight - _new.totalNetWeight || 0, 2)
    if (!artifactObj[_key]) {
      artifactObj[_key] = {
        oldArtifact: _old,
        newArtifact: _new,
        processSummary: arr2obj(_old.processSummaryList || [], 'name'),
        diffLength: _old.length - _new.length || 0,
        diffQuantity,
        diffTotalWeight
      }
    } else {
      artifactObj[_key].oldArtifact.quantity += _old.quantity
      artifactObj[_key].newArtifact.quantity += _new.quantity
      artifactObj[_key].diffQuantity += diffQuantity
      artifactObj[_key].diffTotalWeight += diffTotalWeight
      mergeProcessSummary(artifactObj[_key].processSummary, _old.processSummaryList)
    }

    for (const assemble of artifact.assembleCompareList) {
      const _old = assemble.oldAssemble || {}
      const _new = assemble.newAssemble || {}
      const _key = `${_old.serialNumber}_${_old.specification}_${_old.length}_${_old.netWeight}__${_new.serialNumber}_${_new.specification}_${_new.length}_${_new.netWeight}`
      if (!assembleObj[_key]) {
        assembleObj[_key] = { ...assemble, processSummary: arr2obj(_old.processSummaryList || [], 'name') }
      } else {
        assembleObj[_key].oldAssemble.quantity += _old.quantity
        assembleObj[_key].newAssemble.quantity += _new.quantity
        assembleObj[_key].diffQuantity += assemble.diffQuantity
        assembleObj[_key].diffTotalWeight += assemble.diffTotalWeight
        mergeProcessSummary(assembleObj[_key].processSummary, _old.processSummaryList)
      }
    }

    for (const part of artifact.partCompareResList) {
      const _key = `${part.serialNumber}_${part.specification}_${part.length}_${part.netWeight}_${part.changeType}`
      if (!partObj[_key]) {
        partObj[_key] = { ...part, processSummary: arr2obj(part.processSummaryList || [], 'name') }
      } else {
        partObj[_key].oldQuantity += part.oldQuantity
        partObj[_key].newQuantity += part.newQuantity
        partObj[_key].diffQuantity += part.diffQuantity
        partObj[_key].diffTotalWeight += part.diffTotalWeight
        mergeProcessSummary(partObj[_key].processSummary, part.processSummaryList)
      }
    }
  }

  summaryInfo.value = {
    artifactList: obj2arr(artifactObj),
    assembleList: obj2arr(assembleObj),
    partList: obj2arr(partObj)
  }
}

function mergeProcessSummary(obj, needList) {
  for (const item of needList) {
    if (!obj[item.name]) {
      obj[item.name] = item
    } else {
      obj[item.name].quantity += item.quantity
    }
  }
}

function handleNextStep() {
  if (step.value === 0) {
    const _unHandleQuantity = changeInfo.value.length - handledQuantity.value
    if (_unHandleQuantity) {
      ElMessage.warning(`变更构件还有${_unHandleQuantity}种未处理`)
      return
    }
    handleSummaryData()
  }
  step.value++
}

async function submit() {
  try {
    submitLoading.value = true
    const _artifactList = []
    for (const item of changeInfo.value) {
      // 过滤取消变更的构件
      if (item.artifactHandleStatus & artifactHandleStatusEnum.CANCEL_HANDLE.V) continue
      const assembleList = []
      for (const assemble of item.assembleInfo.needHandleNewList) {
        const changeLinkList = []
        for (const oldSN of assemble.oldSerialNumbers) {
          changeLinkList.push({
            assembleChangeTypeEnum: assemble.handleObj[oldSN].handleType,
            oldAssembleSerialNumber: oldSN,
            quantity: assemble.handleObj[oldSN].quantity
          })
        }
        assembleList.push({
          ...assemble,
          changeLinkList
        })
      }
      const partList = []
      for (const part of item.partCompareResList) {
        partList.push({
          ...part,
          changeTypeEnum: part.changeType
        })
      }
      const _artifact = {
        ...item.newArtifact,
        areaList: item.areaList.map((v) => {
          return { id: v.id, quantity: v.newQuantity }
        }),
        assembleList,
        partList
      }
      _artifactList.push(_artifact)
    }
    const submitInfo = {
      monomerId: props.monomerId,
      projectId: props.projectId,
      newArtifactList: _artifactList
    }
    await changeList(submitInfo)
    ElNotification.success(`提交成功`)
    handleClose()
  } catch (error) {
    console.log(error)
  } finally {
    submitLoading.value = false
  }
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
