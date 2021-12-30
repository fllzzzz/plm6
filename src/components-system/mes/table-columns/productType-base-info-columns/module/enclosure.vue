<template>
  <el-table-column
    v-if="!unShowField.includes('name') && !(unShowNameVal & category) && (isBlank(columns) || columns.visible('name'))"
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
    v-if="!unShowField.includes('serialNumber') && !(unShowSNVal & category) && (isBlank(columns) || columns.visible('serialNumber'))"
    :show-overflow-tooltip="true"
    prop="serialNumber"
    label="编号"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.serialNumber }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('plate') && !(unShowPLVal & category) && (isBlank(columns) || columns.visible('plate'))"
    :show-overflow-tooltip="true"
    prop="plate"
    label="板型"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.plate }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('color') && !(unShowCOVal & category) && (isBlank(columns) || columns.visible('color'))"
    :show-overflow-tooltip="true"
    prop="color"
    label="颜色"
    :width="fixedWidth ? '120px' : ''"
    :min-width="!fixedWidth ? '120px' : ''"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.color }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('material') && !(unShowMAVal & category) && (isBlank(columns) || columns.visible('material'))"
    :show-overflow-tooltip="true"
    prop="material"
    label="材质"
    width="100px"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ row.material }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { mesEnclosureTypeEnum } from '@enum-ms/mes'
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
  // 围护子类型
  category: {
    type: Number
  },
  unShowField: {
    type: Array,
    default: () => []
  }
})

const unShowNameVal = computed(() => {
  return 0
})

const unShowSNVal = computed(() => {
  return mesEnclosureTypeEnum.SANDWICH_BOARD.V
})

const unShowPLVal = computed(() => {
  return mesEnclosureTypeEnum.FOLDING_PIECE.V
})

const unShowCOVal = computed(() => {
  return mesEnclosureTypeEnum.PRESSED_FLOOR_PLATE.V | mesEnclosureTypeEnum.SANDWICH_BOARD.V | mesEnclosureTypeEnum.TRUSS_FLOOR_PLATE.V
})

const unShowMAVal = computed(() => {
  return mesEnclosureTypeEnum.SANDWICH_BOARD.V | mesEnclosureTypeEnum.TRUSS_FLOOR_PLATE.V
})
</script>
