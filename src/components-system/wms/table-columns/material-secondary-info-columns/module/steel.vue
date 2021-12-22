<template>
  <el-table-column v-if="showBrand" prop="brand" label="品牌" align="left" min-width="100px" :fixed="fixed">
    <template #default="{ row }">
      <span v-empty-text>{{ row.brand }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showHeatNoAndBatchNo" prop="heatNoAndBatchNo" :label="heatNoAndBatchNoLabel" align="left" min-width="150px" :fixed="fixed">
    <template #default="{ row }">
      <span v-empty-text>{{ row.heatNoAndBatchNo }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
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
  fixed: {
    // 定位
    type: String
  }
})

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

const showBrand = computed(() => isBlank(props.columns) || props.columns.visible('brand'))
// TODO:初始隐藏会有问题，待查(长宽厚的字段相同情况)
const showHeatNoAndBatchNo = computed(() => props.showBatchNo && (isBlank(props.columns) || props.columns.visible('heatNoAndBatchNo')))
</script>
