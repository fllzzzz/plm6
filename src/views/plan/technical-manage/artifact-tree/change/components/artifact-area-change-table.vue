<template>
  <common-table
    v-if="props.areaList?.length"
    :header-cell-style="() => `font-weight: bold;color: #333333;`"
    :data="tableData"
    style="width: 100%"
  >
    <el-table-column align="center" width="80">
      <template #header>
        <span style="color: #0078fc">共{{ props.areaList?.length }}个区域</span>
      </template>
      <template #default="{ row }">
        <span style="font-weight: bold">{{ row.title }}</span>
      </template>
    </el-table-column>
    <el-table-column v-for="item in props.areaList" :key="item.id" :label="item.name" align="center" min-width="120">
      <template #default="{ row }">
        <cell-compare-preview :value="areaObj?.[item.id]?.[row.field]" v-if="row.isDiff" />
        <span v-else>{{ areaObj?.[item.id]?.[row.field] || '-' }}</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { computed, defineProps } from 'vue'
import cellComparePreview from '@comp-common/cell-compare-preview'

const props = defineProps({
  areaList: {
    type: Object,
    default: () => {}
  }
})

const areaObj = computed(() => {
  const obj = {}
  for (const item of props.areaList) {
    obj[item.id] = {
      ...item,
      diff: (item.oldQuantity || 0) - (item.newQuantity || 0)
    }
  }
  return obj
})

const tableData = computed(() => {
  return [
    { title: '原数量', isDiff: false, field: 'oldQuantity' },
    { title: '现数量', isDiff: false, field: 'newQuantity' },
    { title: '差异', isDiff: true, field: 'diff' }
  ]
})
</script>

<style lang="scss" scoped></style>
