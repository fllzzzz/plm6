<template>
  <el-table-column
    v-if="!unShowField.includes('length') && (isBlank(columns) || columns.visible('length'))"
    :show-overflow-tooltip="true"
    prop="length"
    :label="`长度${unitNewLine ? '\n' : ''}(mm)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="{ k: 'MES_ARTIFACT_L__MM', val: row.length }" v-empty-text></span>
    </template>
  </el-table-column>
  <slot name="quantity" />
  <el-table-column
    v-if="!unShowField.includes('netWeight') && (isBlank(columns) || columns.visible('netWeight'))"
    :show-overflow-tooltip="true"
    prop="netWeight"
    :label="`单净重${unitNewLine ? '\n' : ''}(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="{ k: 'COM_WT__KG', val: row.netWeight }" v-empty-text></span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('totalNetWeight') && (isBlank(columns) || columns.visible('totalNetWeight'))"
    :show-overflow-tooltip="true"
    prop="totalNetWeight"
    :label="`总净重${unitNewLine ? '\n' : ''}(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="{ k: 'COM_WT__KG', val: row.totalNetWeight }" v-empty-text></span>
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
  unitNewLine: {
    type: Boolean,
    default: true
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
  }
})
</script>
