<template>
  <el-table-column
    v-if="!unShowField.includes('name') && (isBlank(columns) || columns.visible('name'))"
    :show-overflow-tooltip="true"
    prop="name"
    label="名称"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <slot name="namePrefix" :row="row"></slot>
      <span v-empty-text>{{ row.name }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('serialNumber') && (isBlank(columns) || columns.visible('serialNumber'))"
    :show-overflow-tooltip="true"
    prop="serialNumber"
    label="构件编号"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.serialNumber }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('specification') && (isBlank(columns) || columns.visible('specification'))"
    :show-overflow-tooltip="true"
    prop="specification"
    label="规格"
    :width="fixedWidth ? '140px' : ''"
    :min-width="!fixedWidth ? '140px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.specification }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('material') && (isBlank(columns) || columns.visible('material'))"
    :show-overflow-tooltip="true"
    prop="material"
    label="材质"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.material }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps } from 'vue'
import { isBlank } from '@/utils/data-type'

defineProps({
  columns: {
    type: Object
  },
  fixed: {
    // 定位
    type: String
  },
  fixedWidth: {
    type: Boolean,
    default: false
  },
  unShowField: {
    type: Array,
    default: () => []
  }
})
</script>
