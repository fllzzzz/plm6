<template>
  <component
    :is="currentView"
    :columns="columns"
    :category="category"
    :fixed="fixed"
    :fixedWidth="fixedWidth"
    :unShowField="unShowField"
    :snClickable="snClickable"
    @drawingPreview="drawingPreview"
  >
    <template #snPrefix="{ row }">
      <slot name="snPrefix" :row="row"></slot>
    </template>
    <template #namePrefix="{ row }">
      <slot name="namePrefix" :row="row"></slot>
    </template>
  </component>
</template>

<script setup>
import { defineProps, computed, defineEmits } from 'vue'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import box from './module/box'
import machinePart from './module/machine-part'
import element from './module/element'
import auxiliaryMaterial from './module/auxiliary-material'

const emit = defineEmits(['drawingPreview'])

const props = defineProps({
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
  unShowField: {
    type: Array,
    default: () => []
  },
  fixed: {
    // 定位
    type: String
  },
  fixedWidth: {
    type: Boolean
  },
  // 编号可点击预览
  snClickable: {
    type: Boolean,
    default: false
  }
})

const currentView = computed(() => {
  switch (props.productType) {
    case bridgeComponentTypeEnum.BOX.V:
      return box
    case bridgeComponentTypeEnum.MACHINE_PART.V:
      return machinePart
    case bridgeComponentTypeEnum.AUXILIARY_MATERIAL.V:
      return auxiliaryMaterial
    case bridgeComponentTypeEnum.CELL.V:
      return element
    default:
      return ''
  }
})

function drawingPreview(row) {
  emit('drawingPreview', row)
}
</script>
