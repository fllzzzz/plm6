<template>
  <el-table-column
    v-if="
      !unShowField.includes('brand') &&
      showBrandVal & productType &&
      !(unShowBrandValEN & category) &&
      (isBlank(columns) || columns.visible('brand'))
    "
    :show-overflow-tooltip="true"
    prop="brand"
    label="品牌"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.brand }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="
      !unShowField.includes('color') &&
      showCOVal & productType &&
      !(unShowCOValEN & category) &&
      (isBlank(columns) || columns.visible('color'))
    "
    :show-overflow-tooltip="true"
    prop="color"
    label="颜色"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.color }}</span>
    </template>
  </el-table-column>
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
      <span>{{ row.drawingNumber }}</span>
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
      <span>{{ row.remark }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { componentTypeEnum, mesEnclosureTypeEnum } from '@enum-ms/mes'
import { isBlank } from '@/utils/data-type'

defineProps({
  productType: {
    type: Number
  },
  // 围护子类型
  category: {
    type: Number
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

const showCOVal = computed(() => {
  return componentTypeEnum.ENCLOSURE.V
})

const showBrandVal = computed(() => {
  return componentTypeEnum.ENCLOSURE.V
})

const unShowCOValEN = computed(() => {
  return mesEnclosureTypeEnum.PRESSED_FLOOR_PLATE.V | mesEnclosureTypeEnum.TRUSS_FLOOR_PLATE.V | mesEnclosureTypeEnum.SANDWICH_BOARD.V
})

const unShowBrandValEN = computed(() => {
  return mesEnclosureTypeEnum.SANDWICH_BOARD.V | mesEnclosureTypeEnum.TRUSS_FLOOR_PLATE.V
})
</script>
