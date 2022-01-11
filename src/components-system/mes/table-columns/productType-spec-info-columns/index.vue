<template>
  <component :is="currentView" :columns="columns" :fixed="fixed" :fixedWidth="fixedWidth" :unShowField="unShowField">
    <template #quantity>
      <slot name="quantity" />
    </template>
  </component>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { componentTypeEnum, mesEnclosureTypeEnum } from '@enum-ms/mes'
import artifact from './module/artifact'
import machinePart from './module/machine-part'
import enclosure from './module/enclosure'
import assemble from './module/assemble'
import pressedPlate from './module/pressed-plate'
import sandwichBoard from './module/sandwich-board'
import trussFloorPlate from './module/truss-floor-plate'
import pressedFloorPlate from './module/pressed-floor-plate'
import foldingPiece from './module/folding-piece'

const props = defineProps({
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
  unShowField: {
    type: Array,
    default: () => []
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
  }
})

const currentView = computed(() => {
  switch (props.productType) {
    case componentTypeEnum.ARTIFACT.V:
      return artifact
    case componentTypeEnum.MACHINE_PART.V:
      return machinePart
    case componentTypeEnum.ENCLOSURE.V:
      if (props.enclosureShowItem) {
        return getEnclosureView()
      } else {
        return enclosure
      }
    case componentTypeEnum.AUXILIARY_MATERIAL.V:
      return ''
    case componentTypeEnum.ASSEMBLE.V:
      return assemble
    default:
      return ''
  }
})

function getEnclosureView() {
  switch (props.category) {
    case mesEnclosureTypeEnum.PRESSED_PLATE.V:
      return pressedPlate
    case mesEnclosureTypeEnum.SANDWICH_BOARD.V:
      return sandwichBoard
    case mesEnclosureTypeEnum.TRUSS_FLOOR_PLATE.V:
      return trussFloorPlate
    case mesEnclosureTypeEnum.PRESSED_FLOOR_PLATE.V:
      return pressedFloorPlate
    case mesEnclosureTypeEnum.FOLDING_PIECE.V:
      return foldingPiece
    default:
      return enclosure
  }
}
</script>
