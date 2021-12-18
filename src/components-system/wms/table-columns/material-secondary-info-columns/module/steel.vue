<template>
  <el-table-column v-if="showBrand" prop="brand" label="品牌" align="left" min-width="100px">
    <template #default="{ row }">
      <span v-empty-text>{{ row.brand }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showHeatNoAndBatchNo" prop="heatNoAndBatchNo" label="炉批号/卷号" align="left" min-width="150px" >
    <template #default="{ row }">
      <span v-empty-text>{{ row.heatNoAndBatchNo }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
  columns: {
    type: Object
  },
  showBatchNo: {
    // 显示炉批号
    type: Boolean,
    default: true
  }
})

const showBrand = computed(() => isBlank(props.columns) || props.columns.visible('brand'))
// TODO:初始隐藏会有问题，待查(长宽厚的字段相同情况)
const showHeatNoAndBatchNo = computed(() => props.showBatchNo && (isBlank(props.columns) || props.columns.visible('heatNoAndBatchNo')))
</script>
