<template>
  <component
    :is="currentView"
    :columns="columns"
    :category="category"
    :fixed="fixed"
    :unitNewLine="unitNewLine"
    :fixedWidth="fixedWidth"
    :unShowField="unShowField"
  />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'
import artifact from './module/artifact'
import machinePart from './module/machine-part'
import enclosure from './module/enclosure'
import assemble from './module/assemble'

const props = defineProps({
  productType: {
    type: Number
  },
  unitNewLine: {
    type: Boolean,
    default: true
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
      return ''
    case componentTypeEnum.ASSEMBLE.V:
      return assemble
    default:
      return ''
  }
})
</script>
