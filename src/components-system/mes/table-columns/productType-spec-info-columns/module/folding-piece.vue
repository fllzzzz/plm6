<template>
  <el-table-column
    v-if="!unShowField.includes('length') && (isBlank(columns) || columns.visible('length'))"
    :show-overflow-tooltip="true"
    prop="length"
    :label="`单长\n(mm)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="{ k: 'MES_ENCLOSURE_L__MM', val: row.length }" v-empty-text></span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('thickness') && (isBlank(columns) || columns.visible('thickness'))"
    :show-overflow-tooltip="true"
    prop="thickness"
    :label="`板厚\n(mm)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="{ k: 'MES_ENCLOSURE_T__MM', val: row.thickness}" v-empty-text></span>
    </template>
  </el-table-column>
  <slot name="quantity" />
  <el-table-column
    v-if="!unShowField.includes('width') && (isBlank(columns) || columns.visible('width'))"
    :show-overflow-tooltip="true"
    prop="width"
    :label="`有效宽度\n(mm)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="{ k: 'MES_ENCLOSURE_W__MM', val: row.width }" v-empty-text></span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('totalArea') && (isBlank(columns) || columns.visible('totalArea'))"
    :show-overflow-tooltip="true"
    prop="totalArea"
    :label="`总面积\n(㎡)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="{ k: 'COM_AREA__M2', val: row.totalArea }" v-empty-text></span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('totalLength') && (isBlank(columns) || columns.visible('totalLength'))"
    :show-overflow-tooltip="true"
    prop="totalLength"
    :label="`总长度\n(m)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="{ k: 'MES_ENCLOSURE_L__M', val: row.totalLength }" v-empty-text></span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('weight') && (isBlank(columns) || columns.visible('weight'))"
    :show-overflow-tooltip="true"
    prop="weight"
    :label="`重量\n(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-to-fixed="{ k: 'COM_WT__KG', val: row.weight }" v-empty-text></span>
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
  }
})
</script>
