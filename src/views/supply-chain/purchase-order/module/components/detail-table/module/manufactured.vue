<template>
  <common-table ref="tableRef" :data="list" :max-height="maxHeight" highlight-current-row row-key="id">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="monomer.name" label="单体" align="center" show-overflow-tooltip min-width="120px" />
    <el-table-column prop="area.name" label="区域" align="center" show-overflow-tooltip min-width="120px" />
    <el-table-column prop="name" label="名称" align="center" show-overflow-tooltip min-width="100px" />
    <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip min-width="100px" />
    <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip min-width="140px" />
    <el-table-column prop="length" label="长度（mm）" align="center" show-overflow-tooltip />
    <el-table-column prop="material" label="材质" align="center" show-overflow-tooltip />
    <el-table-column prop="quantity" label="清单数量" align="center" show-overflow-tooltip />
  </common-table>
</template>

<script setup>
import { defineProps } from 'vue'

defineProps({
  list: {
    type: Array,
    default: () => []
  },
  maxHeight: {
    type: Number
  }
})
</script>

<style lang="scss" scoped></style>
