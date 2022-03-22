<template>
  <el-table-column
    v-if="!unShowField.includes('width') && !(unShowEWVal & category) && (isBlank(columns) || columns.visible('width'))"
    :show-overflow-tooltip="true"
    prop="width"
    :label="`有效宽度${unitNewLine ? '\n' : ''}(mm)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.width }}</span>
    </template>
  </el-table-column>
  <!-- <el-table-column
    v-if="!unShowField.includes('width') && !(unShowWVal & category) && (isBlank(columns) || columns.visible('width'))"
    :show-overflow-tooltip="true"
    prop="width"
    :label="`宽度${unitNewLine?'\n':''}(mm)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{row.width}}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('width') && !(unShowFWVal & category) && (isBlank(columns) || columns.visible('width'))"
    :show-overflow-tooltip="true"
    prop="width"
    :label="`宽度${unitNewLine?'\n':''}(mm)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{row.width}}</span>
    </template>
  </el-table-column> -->
  <el-table-column
    v-if="!unShowField.includes('thickness') && !(unShowTNVal & category) && (isBlank(columns) || columns.visible('thickness'))"
    :show-overflow-tooltip="true"
    prop="thickness"
    :label="`板厚${unitNewLine ? '\n' : ''}(mm)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.thickness }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('length') && (isBlank(columns) || columns.visible('length'))"
    :show-overflow-tooltip="true"
    prop="length"
    :label="`单长${unitNewLine ? '\n' : ''}(mm)`"
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
    v-if="!unShowField.includes('totalArea') && (isBlank(columns) || columns.visible('totalArea'))"
    :show-overflow-tooltip="true"
    prop="totalArea"
    :label="`总面积${unitNewLine ? '\n' : ''}(㎡)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.totalArea }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('totalLength') && (isBlank(columns) || columns.visible('totalLength'))"
    :show-overflow-tooltip="true"
    prop="totalLength"
    :label="`总长度${unitNewLine ? '\n' : ''}(m)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.totalLength }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="!unShowField.includes('weight') && (isBlank(columns) || columns.visible('weight'))"
    :show-overflow-tooltip="true"
    prop="weight"
    :label="`重量${unitNewLine ? '\n' : ''}(kg)`"
    :width="fixedWidth ? '80px' : ''"
    :min-width="!fixedWidth ? '80px' : ''"
    align="center"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span>{{ row.weight }}</span>
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
  // 围护子类型
  category: {
    type: Number
  },
  unShowField: {
    type: Array,
    default: () => []
  }
})

const unShowEWVal = computed(() => {
  // return mesEnclosureTypeEnum.SANDWICH_BOARD.V | mesEnclosureTypeEnum.FOLDING_PIECE.V
  return 0
})

// const unShowWVal = computed(() => {
//   return (
//     mesEnclosureTypeEnum.PRESSED_PLATE.V |
//     mesEnclosureTypeEnum.PRESSED_FLOOR_PLATE.V |
//     mesEnclosureTypeEnum.TRUSS_FLOOR_PLATE.V |
//     mesEnclosureTypeEnum.FOLDING_PIECE.V
//   )
// })

// const unShowFWVal = computed(() => {
//   return (
//     mesEnclosureTypeEnum.PRESSED_PLATE.V |
//     mesEnclosureTypeEnum.PRESSED_FLOOR_PLATE.V |
//     mesEnclosureTypeEnum.TRUSS_FLOOR_PLATE.V |
//     mesEnclosureTypeEnum.SANDWICH_BOARD.V
//   )
// })

const unShowTNVal = computed(() => {
  return mesEnclosureTypeEnum.TRUSS_FLOOR_PLATE.V
})
</script>
