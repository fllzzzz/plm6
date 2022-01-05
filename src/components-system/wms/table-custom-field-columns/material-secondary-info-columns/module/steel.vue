<template>
  <el-table-column
    v-if="showBrand"
    :key="`${field}.brand`"
    :prop="`${field}.brand`"
    label="品牌"
    align="left"
    min-width="100px"
    :fixed="fixed"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <span v-empty-text>{{ getInfo(row, 'brand') }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="showHeatNoAndBatchNo"
    :key="`${field}.heatNoAndBatchNo`"
    :prop="`${field}.heatNoAndBatchNo`"
    :label="heatNoAndBatchNoLabel"
    align="left"
    min-width="150px"
    :fixed="fixed"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <span v-empty-text>{{ getInfo(row, 'heatNoAndBatchNo') }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed, inject } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
  basicClass: {
    type: Number
  },
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
  },
  fixed: {
    // 定位
    type: String
  }
})

const getInfo = inject('getInfo')

// 炉批号label
const heatNoAndBatchNoLabel = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
    case rawMatClsEnum.SECTION_STEEL.V:
      return '炉批号'
    case rawMatClsEnum.STEEL_COIL.V:
      return '卷号'
    default:
      return '炉批号/卷号'
  }
})

const showBrand = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.brand`))
const showHeatNoAndBatchNo = computed(
  () => props.showBatchNo && (isBlank(props.columns) || props.columns.visible(`${props.field}.heatNoAndBatchNo`))
)
</script>
