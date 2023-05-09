<template>
  <common-table :data="list" :max-height="maxHeight" empty-text="暂无数据" style="width: 100%">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" />
    <el-table-column :show-overflow-tooltip="true" prop="monomer.name" label="单体" />
    <el-table-column :show-overflow-tooltip="true" prop="fullClassName" label="辅材类别" width="280px" />
    <el-table-column prop="unit" label="单位" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="color" label="颜色" />
    <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" />
    <el-table-column prop="showQuantity" label="数量" align="center" />
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
    type: [String, Number],
    default: undefined
  }
})
</script>
