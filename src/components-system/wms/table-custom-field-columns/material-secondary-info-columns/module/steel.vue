<template>
  <el-table-column v-if="showBrand" :prop="`${field}.brand`" label="品牌" align="left" min-width="100px">
    <template #default="{ row }">
      <span v-empty-text>{{ getInfo(row, 'brand') }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showHeatNoAndBatchNo" :prop="`${field}.heatNoAndBatchNo`" label="炉批号/卷号" align="left" min-width="150px">
    <template #default="{ row }">
      <span v-empty-text>{{ getInfo(row, 'heatNoAndBatchNo') }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed, inject } from 'vue'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
  columns: {
    type: Object
  },
  showBatchNo: {
    // 显示炉批号
    type: Boolean,
    default: true
  },
  field: {
    // 字段
    type: String,
    default: 'material'
  }
})

const getInfo = inject('getInfo')

const showBrand = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.brand`))
const showHeatNoAndBatchNo = computed(() => props.showBatchNo && (isBlank(props.columns) || props.columns.visible(`${props.field}.heatNoAndBatchNo`)))
</script>
