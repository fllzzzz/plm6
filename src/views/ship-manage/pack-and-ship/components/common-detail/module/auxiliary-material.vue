<template>
  <common-table :data="list" :show-empty-symbol="false" :max-height="maxHeight" empty-text="暂无数据" style="width: 100%">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column :show-overflow-tooltip="true" prop="monomer.name" label="单体">
      <template #default="{ row }">
        <span>{{ row.monomer ? row.monomer?.name : '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="isSuspend" :show-overflow-tooltip="true" prop="area.name" label="区域">
      <template #default="{ row }">
        <span>{{ row.area ? row.area?.name : '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" />
    <el-table-column :show-overflow-tooltip="true" prop="name" label="名称" width="280px" />
    <el-table-column prop="measureUnit" label="单位" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="color" label="颜色">
      <template #default="{ row }">
        <span>{{ row.color ? row.color : '-' }}</span>
      </template>
    </el-table-column>
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
  isSuspend: {
    type: Boolean,
    default: false
  },
  maxHeight: {
    type: [String, Number],
    default: undefined
  }
})
</script>
