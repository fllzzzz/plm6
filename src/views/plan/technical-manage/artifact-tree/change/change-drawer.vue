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
import { defineProps, defineEmits, computed, ref, reactive, provide } from 'vue'

import { arr2obj, obj2arr } from '@/utils/convert/type'
import { toPrecision } from '@/utils/data-type'
import { changeTypeEnum } from './components/common.js'

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
    v.assembleCompareResObj = {}
    _list.push(v)
    changeInfoMap.value.set(v.newArtifact.serialNumber, v)
  }
  console.log({ _list })
  changeInfo.value = _list
}

// 处理部件列表
function handleAssembleList() {

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
      changeType = n.diffQuantity > 0 ? changeTypeEnum.MINUS.V : changeTypeEnum.ADD.V
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
