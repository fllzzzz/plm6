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
      <component ref="componentRef" :is="currentView" :height-style="heightStyle" />
    </template>
  </common-drawer>
</template>

<script setup>
import { changeList } from '@/api/plan/technical-manage/artifact-tree'
import { defineProps, defineEmits, computed, ref, reactive, provide, watchEffect } from 'vue'
import { ElMessage, ElNotification } from 'element-plus'

import { isBlank } from '@/utils/data-type'
import { artifactHandleStatusEnum, assembleHandleMethodEnum } from '@/components-system/plan/change/common.js'

// import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useHandleChangeList from '@compos/plan/change/use-handle-change-list'
// import useHandelSummaryData from '@compos/plan/change/use-handle-summary-data'
import commonStep from '@comp-common/common-step/index'
import mHandle from './module/handle'
// import mSummary from './module/summary' // 变更汇总
// import mTechnicalUpload from './module/technical-upload' // 技术变更上传
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
// const { maxHeight, heightStyle } = useMaxHeight(
//   {
//     extraBox: ['.el-drawer__header', '.step-content'],
//     wrapperBox: ['.el-drawer__body'],
//     navbar: false,
//     clientHRepMainH: true,
//     extraHeight: 50
//   },
//   drawerVisible
// )

const { handleAssembleList, handleCompareAssembleList, handleComparePartList } = useHandleChangeList()
// const { handleSummaryData } = useHandelSummaryData()

const componentRef = ref()
const stepOptions = reactive([{ title: '变更处理' }, { title: '变更原因填写' }])
const stepComponent = [mHandle, mReason]
const step = ref(0)
const submitLoading = ref(false)
const form = ref({})
const changeInfo = ref([])
const changeInfoMap = ref(new Map())
provide('form', form)
provide('changeInfo', changeInfo)
provide('changeInfoMap', changeInfoMap)
provide('originChangeInfo', props.originChangeInfo)

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
    v.partCompareList = handleComparePartList(v.oldArtifact.partList || [], v.newArtifact.partList || [])
    v.assembleInfo = handleAssembleList(v.oldArtifact.assembleList || [], v.newArtifact.assembleList || [])
    v.artifactHandleStatus = getArtifactInitStatus(v)
    v.boolTag = false
    v.boolDel = false
    v.foldOpened = true
    artifactWatch(v)
    _list.push(v)
    changeInfoMap.value.set(v.newArtifact.serialNumber, v)
  }
  console.log({ _list })
  changeInfo.value = _list
}

function artifactWatch(item) {
  watchEffect(() => {
    const { assembleCompareList, isAssembleHandled } = handleCompareAssembleList(item.assembleInfo)
    item.assembleCompareList = assembleCompareList
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

async function handleNextStep() {
  if (step.value === 0) {
    const _unHandleQuantity = changeInfo.value.length - handledQuantity.value
    if (_unHandleQuantity) {
      ElMessage.warning(`变更构件还有${_unHandleQuantity}种未处理`)
      return
    }
    // handleSummaryData(changeInfo.value)
  }
  if (componentRef.value?.validate) {
    const validate = await componentRef.value.validate()
    if (!validate) return
  }
  step.value++
}

async function submit() {
  if (componentRef.value?.validate) {
    const validate = await componentRef.value.validate()
    if (!validate) return
  }
  try {
    submitLoading.value = true
    const _artifactList = []
    for (const item of changeInfo.value) {
      // 过滤取消变更的构件
      if (item.artifactHandleStatus & artifactHandleStatusEnum.CANCEL_HANDLE.V) continue
      const assembleList = []
      for (const assemble of item.assembleInfo?.needHandleNewList) {
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
      // 处理完全相同部件的数据
      for (const assemble of item.assembleInfo?.sameList) {
        assembleList.push({
          ...assemble.newAssemble,
          changeLinkList: [
            {
              assembleChangeTypeEnum: assembleHandleMethodEnum.KEEP.V,
              oldAssembleSerialNumber: assemble.oldAssemble?.serialNumber,
              quantity: assemble.oldAssemble?.quantity
            }
          ]
        })
      }
      // 处理数量不同的部件
      for (const assemble of item.assembleInfo?.amList) {
        assembleList.push({
          ...assemble.newAssemble,
          changeLinkList: [
            {
              assembleChangeTypeEnum: assembleHandleMethodEnum.KEEP.V,
              oldAssembleSerialNumber: assemble.oldAssemble?.serialNumber,
              quantity: assemble.oldAssemble?.quantity
            }
          ]
        })
      }
      // const partList = []
      // for (const part of item.partCompareList) {
      //   partList.push({
      //     ...part,
      //     changeTypeEnum: part.changeType
      //   })
      // }
      const _artifact = {
        ...item.newArtifact,
        areaList: item.areaList.map((v) => {
          return { id: v.id, quantity: v.newQuantity }
        }),
        assembleList
        // partList
      }
      _artifactList.push(_artifact)
    }
    const attachmentIds = form.value.attachments?.length ? form.value.attachments.map((v) => v.id) : undefined
    const submitInfo = {
      ...form.value,
      monomerId: props.monomerId,
      projectId: props.projectId,
      newArtifactList: _artifactList,
      attachmentIds
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
