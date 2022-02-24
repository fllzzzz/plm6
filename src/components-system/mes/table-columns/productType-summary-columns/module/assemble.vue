<template>
  <el-table-column
    v-if="!unShowField.includes('serialNumber') && (isBlank(columns) || columns.visible('serialNumber'))"
    :show-overflow-tooltip="true"
    prop="serialNumber"
    label="组立号"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
    align="center"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.serialNumber }}</span>
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
      <span v-empty-text>{{ row.quantity }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('netWeight') && (isBlank(columns) || columns.visible('netWeight'))"
    :show-overflow-tooltip="true"
    prop="netWeight"
    :label="`重量${unitNewLine ? '\n' : ''}(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="{ k: 'COM_WT__KG', val: row.netWeight }" v-empty-text></span>
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
    type: Boolean
  },
  unShowField: {
    type: Array,
    default: () => []
  },
  unitNewLine: {
    type: Boolean,
    default: true
  },
  // 围护子类型
  category: {
    type: Number
  }
})
</script>
