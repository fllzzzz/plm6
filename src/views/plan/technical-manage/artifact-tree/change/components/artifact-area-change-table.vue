<template>
  <common-table v-if="props.areaList?.length" :data="tableData" :max-height="maxHeight" style="width: 100%">
    <el-table-column align="center" width="80">
      <template #header>
        <span>共{{ props.areaList?.length }}个区域</span>
      </template>
      <template #default="{ row }">
        <span>{{ row.title }}</span>
      </template>
    </el-table-column>
    <el-table-column v-for="item in props.areaList" :key="item.id" :label="item.name" align="center" width="120">
      <template #default="{ row }">
        <span v-if="row.field">{{ areaObj?.[item.id]?.[row.field] || '-' }}</span>
        <span v-else>{{ areaObj?.[item.id]?.[row.field] || '-' }}</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { computed, defineProps } from 'vue'

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
    { title: '原数量', field: 'oldQuantity' },
    { title: '现数量', field: 'newQuantity' },
    { title: '差异', field: 'diff' }
  ]
})
</script>

<style lang="scss" scoped></style>
