<template>
  <common-dialog
    title="生产变更预览"
    custom-class="schedule-change-preview"
    v-model="dialogVisible"
    :closeOnClickModal="false"
    width="1100px"
    :before-close="handleClose"
  >
    <template #titleRight>
      <common-button :loading="loading" :disabled="isBlank(modifiedList)" type="primary" size="mini" @click="submit">
        确认提交
      </common-button>
    </template>
    <common-table
      ref="tableRef"
      :summary-method="getSummaries"
      show-summary
      :data="modifiedList"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns showProductionLine showFactory showProcess />
      <el-table-column prop="reportTypeText" :show-overflow-tooltip="true" label="上报类型" align="center">
        <template #default="{ row }">
          <span>{{ row.reportTypeText }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="completeTime" :show-overflow-tooltip="true" label="上报时间" align="center" width="160">
        <template #default="{ row }">
          <span>{{ row.completeTime }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="quantity" :show-overflow-tooltip="true" label="生产数量" align="center">
        <template #default="{ row }">
          <span>{{ row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="canHandleQuantity" :show-overflow-tooltip="true" label="可处理数量" align="center">
        <template #default="{ row }">
          <span>{{ row.canHandleQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="dealQuantity" :show-overflow-tooltip="true" label="处理数量" align="center">
        <template #default="{ row }">
          <span>{{ row.dealQuantity }}</span>
        </template>
      </el-table-column>
    </common-table>
    <el-divider>
      <span class="title">排产变更</span>
    </el-divider>
    <common-table v-loading="previewLoading" :summary-method="getSummariesPreview" show-summary :data="previewList" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns showProductionLine showFactory />
      <productType-base-info-columns :productType="info?.productType" />
      <el-table-column prop="schedulingQuantity" :show-overflow-tooltip="true" label="排产数量" align="center">
        <template #default="{ row }">
          <span>{{ row.schedulingQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="reduceQuantity" :show-overflow-tooltip="true" label="处理数量" align="center">
        <template #default="{ row }">
          <span>{{ row.reduceQuantity }}</span>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { exceptionChange, changePreview } from '@/api/mes/changed-manage/common'
import { defineEmits, defineProps, ref, computed } from 'vue'

import { isBlank } from '@data-type'
import { tableSummary } from '@/utils/el-extra'
import { ElNotification } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import productTypeBaseInfoColumns from '@comp-mes/table-columns/productType-base-info-columns'

const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  list: {
    type: Array,
    default: () => []
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetchPreview })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.schedule-change-preview',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  dialogVisible
)

const loading = ref(false)
const previewLoading = ref(false)
const previewList = ref([])
const modifiedList = computed(() => {
  return props.list.filter((v) => v.dealQuantity)
})

function getSummaries(param) {
  return tableSummary(param, { props: ['quantity', 'canHandleQuantity', 'dealQuantity'] })
}

function getSummariesPreview(param) {
  return tableSummary(param, { props: ['reduceQuantity'] })
}

async function fetchPreview() {
  try {
    previewLoading.value = true
    const submitList = modifiedList.value.map((v) => {
      const o = {}
      o.id = v.id
      o.quantity = v.dealQuantity
      return o
    })
    const { content } = await changePreview({
      productType: props.info.productType,
      abnormalId: props.info.id,
      reportList: submitList
    })
    previewList.value = content
  } catch (error) {
    console.log('变更预览', error)
  } finally {
    previewLoading.value = false
  }
}

async function submit() {
  try {
    loading.value = true
    const submitList = modifiedList.value.map((v) => {
      const o = {}
      o.id = v.id
      o.quantity = v.dealQuantity
      return o
    })
    await exceptionChange({
      productType: props.info.productType,
      abnormalId: props.info.id,
      voList: submitList
    })
    ElNotification({
      title: '变更处理成功',
      type: 'success',
      duration: 2500
    })
    handleClose()
    emit('success')
  } catch (error) {
    console.log('变更处理失败', error)
  } finally {
    loading.value = false
  }
}
</script>
