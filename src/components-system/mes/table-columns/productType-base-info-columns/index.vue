<template>
  <component :is="currentView" :columns="columns" :category="category" :fixed="fixed" :fixedWidth="fixedWidth" :unShowField="unShowField" :snClickable="snClickable" @drawingPreview="drawingPreview">
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
import { componentTypeEnum } from '@enum-ms/mes'
import artifact from './module/artifact'
import machinePart from './module/machine-part'
import enclosure from './module/enclosure'
import assemble from './module/assemble'
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
    case componentTypeEnum.ARTIFACT.V:
      return artifact
    case componentTypeEnum.MACHINE_PART.V:
      return machinePart
    case componentTypeEnum.ENCLOSURE.V:
      return enclosure
    case componentTypeEnum.AUXILIARY_MATERIAL.V:
      return auxiliaryMaterial
    case componentTypeEnum.ASSEMBLE.V:
      return assemble
    default:
      return ''
  }
})

function drawingPreview(row) {
  emit('drawingPreview', row)
}
</script>
