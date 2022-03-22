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
      <span>{{ row.length }}</span>
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
      <span>{{ row.netWeight }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('grossWeight') && (isBlank(columns) || columns.visible('grossWeight'))"
    :show-overflow-tooltip="true"
    prop="grossWeight"
    :label="`单毛重${unitNewLine ? '\n' : ''}(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.grossWeight }}</span>
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
      <span>{{ row.totalNetWeight }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('totalGrossWeight') && (isBlank(columns) || columns.visible('totalGrossWeight'))"
    :show-overflow-tooltip="true"
    prop="totalGrossWeight"
    :label="`总毛重${unitNewLine ? '\n' : ''}(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.totalGrossWeight }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('surfaceArea') && (isBlank(columns) || columns.visible('surfaceArea'))"
    :show-overflow-tooltip="true"
    prop="surfaceArea"
    :label="`面积${unitNewLine ? '\n' : ''}(㎡)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.surfaceArea }}</span>
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
