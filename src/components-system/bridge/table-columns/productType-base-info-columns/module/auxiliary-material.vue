<template>
  <el-table-column
    v-if="!unShowField.includes('serialNumber') && (isBlank(columns) || columns.visible('serialNumber'))"
    :show-overflow-tooltip="true"
    prop="serialNumber"
    label="编号"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <slot name="snPrefix" :row="row"></slot>
      <span>{{ row.serialNumber }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('classifyName') && (isBlank(columns) || columns.visible('classifyName'))"
    :show-overflow-tooltip="true"
    prop="classifyName"
    label="名称"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <slot name="namePrefix" :row="row"></slot>
      <el-tooltip :content="row.classifyFullName" :disabled="!row.classifyFullName" :show-after="500" placement="top">
        <span>{{ row.classifyName }}</span>
      </el-tooltip>
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
      <span>{{ row.specification }}</span>
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
  snClickable: {
    type: Boolean,
    default: false
  },
  // 围护子类型
  category: {
    type: Number
  },
  unShowField: {
    type: Array,
    default: () => []
  }
})
</script>
