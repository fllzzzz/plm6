<template>
  <el-table-column
    v-if="!unShowField.includes('name') && (isBlank(columns) || columns.visible('name'))"
    :show-overflow-tooltip="true"
    prop="name"
    label="名称"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
    align="center"
  >
    <template #default="{ row }">
      <slot name="namePrefix" :row="row"></slot>
      <span>{{ row.name }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('quantity') && (isBlank(columns) || columns.visible('quantity'))"
    :show-overflow-tooltip="true"
    prop="quantity"
    label="数量(件)"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.quantity }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('totalNetWeight') && (isBlank(columns) || columns.visible('totalNetWeight'))"
    :show-overflow-tooltip="true"
    prop="totalNetWeight"
    :label="`重量${unitNewLine ? '\n' : ''}(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{row.totalNetWeight}}</span>
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
  // 围护子类型
  category: {
    type: Number
  },
  unitNewLine: {
    type: Boolean,
    default: true
  },
  unShowField: {
    type: Array,
    default: () => []
  }
})
</script>
