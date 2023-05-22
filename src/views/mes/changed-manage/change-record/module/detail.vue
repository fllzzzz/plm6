<template>
  <common-drawer
    ref="drawerRef"
    title="清单变更"
    v-model="drawerVisible"
    direction="rtl"
    :content-loading="detailLoading"
    :before-close="handleClose"
    size="100%"
    custom-class="change-record-detail"
  >
    <template #titleAfter>
      <el-tag effect="plain" size="medium">项目：{{ info.project }}</el-tag>
      <el-tag effect="plain" size="medium">单体：{{ info.monomer?.name }}</el-tag>
    </template>
    <template #titleRight> </template>
    <template #content>
      <el-card class="step-content">
        <div class="common-step-warp">
          <common-step v-model="step" :options="stepOptions" finish-status="success" :style="`width: ${200 * stepOptions.length}px`" />
        </div>
        <span class="step-btn">
          <common-button size="mini" plain :disabled="step === 0" @click="step--">上一步</common-button>
          <common-button size="mini" plain :disabled="step === stepOptions.length - 1" @click="handleNextStep">下一步</common-button>
          <common-button :loading="submitLoading" size="mini" type="warning" :disabled="step !== stepOptions.length - 1" @click="submit">
            确认变更
          </common-button>
        </span>
      </el-card>
      <component v-loading="contentLoading" :is="currentView" :height-style="heightStyle" />
    </template>
  </common-drawer>
</template>

<script setup>
import { changeDetail, handleChange, getChangeTaskList } from '@/api/mes/changed-manage/change-record'
import { defineProps, defineEmits, computed, ref, provide, watch } from 'vue'
import { ElMessage, ElNotification } from 'element-plus'

import { isNotBlank } from '@data-type/index'
import { changeTypeEnum } from '@/components-system/plan/change/common.js'
import { createUniqueString } from '@/utils/data-type/string'
import { DP } from '@/settings/config'

// import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useHandleChangeList from '@compos/plan/change/use-handle-change-list'
import useHandelSummaryData from '@compos/plan/change/use-handle-summary-data'
import commonStep from '@comp-common/common-step/index'
import mReason from './change-detail/reason'
import mHandleInfo from './change-detail/handle-info'
// import mChangeSummary from './change-detail/change-summary'
// import mTaskChangeInfo from './change-detail/task-change-info'
import artifactRescheduling from './change-detail/artifact-rescheduling.vue'
import assembleRescheduling from './change-detail/assemble-rescheduling.vue'
// import artifactPointWorkScheduling from './change-detail/artifact-point-work-scheduling.vue'
import assemblePointWorkScheduling from './change-detail/assemble-point-work-scheduling.vue'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

// 表格高度处理
// const { maxHeight, heightStyle } = useMaxHeight(
//   {
//     mainBox: '.change-record-detail',
//     extraBox: ['.el-drawer__header', '.step-content'],
//     wrapperBox: ['.el-drawer__body'],
//     clientHRepMainH: true
//   },
//   () => computed(() => !drawerVisible.value)
// )

const { handleAssembleList, handleCompareAssembleList } = useHandleChangeList()
const { handleSummaryData } = useHandelSummaryData()

const step = ref(0)
const detailLoading = ref(false)
const submitLoading = ref(false)
const contentLoading = ref(false)
const changeInfo = ref()
const taskInfo = ref({})
const schedulingInfo = ref({})
const schedulingHandle = ref({
  artifact: {},
  artifactPointWork: {},
  assemble: {},
  assemblePointWork: {}
})
provide('changeInfo', changeInfo)
provide('taskInfo', taskInfo)
provide('schedulingInfo', schedulingInfo)
provide('schedulingHandle', schedulingHandle)

const stepOptions = computed(() => {
  const baseOptions = [
    { title: '变更原因', comp: mReason },
    { title: '变更详情', comp: mHandleInfo }
    // { title: '变更总览', comp: mChangeSummary },
    // { title: '变更文件总览', comp: mTaskChangeInfo }
    // { title: '任务变更总览', comp: mTaskChangeInfo }
  ]
  if (schedulingInfo.value?.artifactList?.length) {
    baseOptions.push({ title: '构件任务变更下发', comp: artifactRescheduling })
  }
  // if (schedulingInfo.value?.pointWorkTaskArtifactList?.length) {
  //   baseOptions.push({ title: '构件点工下发', comp: artifactPointWorkScheduling })
  // }
  if (schedulingInfo.value?.assembleList?.length) {
    baseOptions.push({ title: '部件任务变更下发', comp: assembleRescheduling })
  }
  if (schedulingInfo.value?.pointWorkTaskAssembleList?.length) {
    baseOptions.push({ title: '部件点工下发', comp: assemblePointWorkScheduling })
  }
  return baseOptions
})

const currentView = computed(() => stepOptions.value[step.value].comp)

// 监听step变化，滚动到当前step
watch(
  () => step.value,
  (val) => {
    if (document.querySelector('.common-step-warp')) {
      const dom = document.querySelector('.common-step-warp').querySelectorAll('.el-step')[val]
      dom.scrollIntoView({ behavior: 'smooth' })
      // 当step为0时，滚动到顶部
      if (val === 0) {
        document.querySelector('.common-step-warp').scrollTo(0, 0)
      }
    }
  }
)

function showHook() {
  step.value = 0
  fetchChangeDetail()
}

async function fetchChangeDetail() {
  try {
    changeInfo.value = {}
    detailLoading.value = true
    const { changeDTOList, attachmentList, changeReasonTypeEnum, remark } = await changeDetail(props.info.id)
    const changeList = changeDTOList.map((v) => {
      if (v?.changePartList?.length) {
        v.partCompareList = handleComparePartList(v.changePartList)
      }
      v.assembleInfo = handleAssembleList(v.oldArtifact.assembleList || [], v.newArtifact.assembleList || [])
      const { assembleCompareList } = handleCompareAssembleList(v.assembleInfo)
      v.assembleCompareList = assembleCompareList
      v.foldOpened = true
      return v
    })
    changeInfo.value = { attachments: attachmentList, changeReasonTypeEnum, remark, changeList }
    await fetchChangeTaskList()
  } catch (error) {
    console.log(error, '获取详情失败')
  } finally {
    detailLoading.value = false
  }
}

// async function fetchTaskDetail() {
//   taskInfo.value = {}
//   try {
//     contentLoading.value = true
//     const data = await taskDetail(props.info.id)
//     taskInfo.value = data
//   } catch (error) {
//     console.log(error, '获取详情失败')
//   } finally {
//     contentLoading.value = false
//   }
// }

async function fetchChangeTaskList() {
  // 重置
  schedulingInfo.value = {}
  schedulingHandle.value = {
    artifact: {},
    artifactPointWork: {},
    assemble: {},
    assemblePointWork: {}
  }
  try {
    submitLoading.value = true
    const data = await getChangeTaskList(props.info.id)
    for (const key in data) {
      if (data[key]?.length) {
        data[key].forEach((v) => {
          v.originQuantity = v.quantity
          v.rowKey = createUniqueString()
        })
      }
    }
    schedulingInfo.value = {
      artifactList: data.artifactList || [],
      pointWorkTaskArtifactList: data.pointWorkTaskArtifactList || [],
      assembleList: data.assembleList || [],
      pointWorkTaskAssembleList: data.pointWorkTaskAssembleList || []
    }
  } catch (error) {
    console.log(error, '获取变更list失败')
  } finally {
    submitLoading.value = false
  }
}

function handleComparePartList(list) {
  return list.map((o) => {
    o.changeType = o.changeTypeEnum
    if (o.changeType & changeTypeEnum.NEW.V) {
      o.newQuantity = o.changeQuantity || o.quantity
      o.oldQuantity = o.oldQuantity || 0
      o.diffQuantity = -(o.changeQuantity || o.quantity)
    }
    if (o.changeType & changeTypeEnum.DEL.V) {
      o.newQuantity = o.changeQuantity || 0
      o.oldQuantity = o.oldQuantity || o.quantity
      o.diffQuantity = o.oldQuantity || o.quantity
    }
    if (o.diffQuantity && o.netWeight) {
      o.diffTotalWeight = (o.diffQuantity * o.netWeight).toFixed(DP.COM_WT__KG)
    }
    return o
  })
}

function handleNextStep() {
  if (step.value === 1) {
    handleSummaryData(changeInfo.value?.changeList)
  }
  // if (step.value === 2) {
  //   fetchTaskDetail()
  // }
  step.value++
}

async function submit() {
  try {
    submitLoading.value = true
    const assembleTaskList = []
    if (schedulingInfo.value?.assembleList?.length) {
      const _handleAssemble = schedulingHandle.value.assemble
      if (schedulingHandle.value?.assembleNeedSchedulingList?.length) {
        ElMessage.warning('部件任务变更尚未全部处理完，请处理部件任务变更下发！')
        return
      }
      if (isNotBlank(_handleAssemble)) {
        for (const key in _handleAssemble) {
          for (const item in _handleAssemble[key]?.handleObj) {
            const _handleObj = _handleAssemble[key]?.handleObj?.[item] || {}
            assembleTaskList.push({
              changeAssembleId: _handleAssemble[key].changeAssembleId,
              quantity: _handleObj.needSchedulingQuantity,
              groupsId: _handleObj.groupsId,
              askCompleteTime: _handleObj.askCompleteTime
            })
          }
        }
      }
    }
    await handleChange({
      changeId: props.info.id,
      assembleTaskList
    })
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
.common-step-warp {
  &::-webkit-scrollbar {
    width: 0 !important;
    height: 0 !important;
  }

  width: calc(100% - 240px);
  overflow: auto;

  ::v-deep(.el-step) {
    width: 200px !important;
  }
}
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
