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
        <common-step v-model="step" :options="stepOptions" space="16.6%" finish-status="success" style="width: calc(100% - 240px)" />
        <span class="step-btn">
          <common-button size="mini" plain :disabled="step === 0" @click="step--">上一步</common-button>
          <common-button size="mini" plain :disabled="step === stepOptions.length - 1" @click="handleNextStep">下一步</common-button>
          <common-button :loading="submitLoading" size="mini" type="warning" :disabled="step !== stepOptions.length - 1" @click="submit">
            确认变更
          </common-button>
        </span>
      </el-card>
      <component :is="currentView" :height-style="heightStyle" />
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/mes/changed-manage/change-record'
import { defineProps, defineEmits, computed, reactive, ref, provide } from 'vue'
import { ElMessage, ElNotification } from 'element-plus'

import { changeTypeEnum } from '@/components-system/plan/change/common.js'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useHandleChangeList from '@compos/plan/change/use-handle-change-list'
import useHandelSummaryData from '@compos/plan/change/use-handle-summary-data'
import commonStep from '@comp-common/common-step/index'
import mReason from './change-detail/reason'
import mHandleInfo from './change-detail/handle-info'
import mChangeSummary from './change-detail/change-summary'

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
const { maxHeight, heightStyle } = useMaxHeight(
  {
    mainBox: '.change-record-detail',
    extraBox: ['.el-drawer__header', '.step-content'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    extraHeight: 50
  },
  () => computed(() => !drawerVisible.value)
)

const { handleAssembleList, handleCompareAssembleList } = useHandleChangeList()
const { handleSummaryData } = useHandelSummaryData()

const stepOptions = reactive([
  { title: '变更原因' },
  { title: '变更详情' },
  { title: '变更总览' },
  { title: '任务变更总览' },
  { title: '变更文件总览' },
  { title: '变更任务重新排产' }
])
const stepComponent = [mReason, mHandleInfo, mChangeSummary, null]
const step = ref(0)
const detailLoading = ref(false)
const submitLoading = ref(false)
const rowDetail = ref()
provide('changeInfo', rowDetail)

const currentView = computed(() => stepComponent[step.value])

function showHook() {
  step.value = 0
  fetchDetail()
}

async function fetchDetail() {
  try {
    detailLoading.value = true
    const content = await detail(props.info.id)
    rowDetail.value = content.map((v) => {
      if (v?.changePartList?.length) {
        v.partCompareList = handleComparePartList(v.changePartList)
      }
      v.assembleInfo = handleAssembleList(v.oldArtifact.assembleList || [], v.newArtifact.assembleList || [])
      const { assembleCompareList } = handleCompareAssembleList(v.assembleInfo)
      v.assembleCompareList = assembleCompareList
      return v
    })
  } catch (error) {
    console.log(error, '获取详情失败')
  } finally {
    detailLoading.value = false
  }
}

function handleComparePartList(list) {
  return list.map((o) => {
    o.changeType = o.changeTypeEnum
    if (o.changeType & changeTypeEnum.NEW.V) {
      o.newQuantity = o.quantity
      o.oldQuantity = 0
      o.diffQuantity = -o.quantity
      o.diffTotalWeight = -o.totalNetWeight
    }
    if (o.changeType & changeTypeEnum.DEL.V) {
      o.newQuantity = 0
      o.oldQuantity = o.quantity
      o.diffQuantity = o.quantity
      o.diffTotalWeight = o.totalNetWeight
    }
    return o
  })
}

function handleNextStep() {
  if (step.value === 1) {
    handleSummaryData(rowDetail.value)
  }
  step.value++
}

async function submit() {
  try {
    submitLoading.value = true
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
