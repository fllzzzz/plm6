<template>
  <el-table-column
    v-if="!(unShowVal&productType) && (isBlank(columns) || columns.visible('name'))"
    :show-overflow-tooltip="true"
    prop="name"
    label="名称"
    width="120px"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.name }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="isBlank(columns) || columns.visible('serialNumber')"
    :show-overflow-tooltip="true"
    prop="serialNumber"
    label="编号"
    width="120px"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.serialNumber }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'
import { isBlank } from '@/utils/data-type'

defineProps({
  productType: {
    type: Number
  },
  columns: {
    type: Object
  },
  fixed: {
    // 定位
    type: String
  }
})

const unShowVal = computed(() => {
  return componentTypeEnum.MACHINE_PART.V | componentTypeEnum.ASSEMBLE.V
})
</script>
