<template>
  <productType-base-info-columns
    :productType="productType"
    :category="category"
    :columns="columns"
    :fixed="fixed"
    :unShowField="unShowField"
    :fixedWidth="fixedWidth"
  />
  <productType-spec-info-columns
    :productType="productType"
    :category="category"
    :columns="columns"
    :enclosureShowItem="enclosureShowItem"
    :fixed="fixed"
    :unShowField="unShowField"
    :fixedWidth="fixedWidth"
  >
   <template #quantity>
      <slot name="quantity" />
    </template>
  </productType-spec-info-columns>
  <el-table-column
    v-if="!unShowField.includes('drawingNumber') && !(unShowDNVal & productType) && (isBlank(columns) || columns.visible('drawingNumber'))"
    :show-overflow-tooltip="true"
    prop="drawingNumber"
    label="图号"
    :width="fixedWidth ? '140px' : ''"
    :min-width="!fixedWidth ? '140px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.drawingNumber }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('remark') && (isBlank(columns) || columns.visible('remark'))"
    :show-overflow-tooltip="true"
    prop="remark"
    label="备注"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.remark }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'
import { isBlank } from '@/utils/data-type'
import productTypeBaseInfoColumns from '../productType-base-info-columns'
import productTypeSpecInfoColumns from '../productType-spec-info-columns'

defineProps({
  productType: {
    type: Number
  },
  // 围护子类型
  category: {
    type: Number
  },
  enclosureShowItem: {
    type: Boolean,
    default: false
  },
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

const unShowDNVal = computed(() => {
  return componentTypeEnum.ENCLOSURE.V
})
</script>
