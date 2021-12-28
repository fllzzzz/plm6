<template>
  <component :is="currentView" :query="query" :product-type="productType" @to-query="toQuery" />
</template>

<script setup>
import { defineEmits, defineProps, computed, watchEffect, ref } from 'vue'
import { componentTypeEnum, mesEnclosureTypeEnum } from '@enum-ms/mes'

import artifact from './module/artifact'
import assemble from './module/assemble'
import enclosure from './module/enclosure'
import sandwichBoard from './module/sandwich-board'

const emit = defineEmits(['to-query'])

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
  query: {
    type: Object,
    default: () => {
      return {}
    }
  },
  toQuery: {
    type: Function
  }
})

const currentView = computed(() => {
  switch (props.productType) {
    case componentTypeEnum.ARTIFACT.V:
      return artifact
    case componentTypeEnum.MACHINE_PART.V:
      return artifact
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
    // case mesEnclosureTypeEnum.PRESSED_PLATE.V:
    //   return enclosure
    case mesEnclosureTypeEnum.SANDWICH_BOARD.V:
      return sandwichBoard
    // case mesEnclosureTypeEnum.TRUSS_FLOOR_PLATE.V:
    //   return enclosure
    // case mesEnclosureTypeEnum.PRESSED_FLOOR_PLATE.V:
    //   return enclosure
    // case mesEnclosureTypeEnum.FOLDING_PIECE.V:
    //   return enclosure
    default:
      return enclosure
  }
}

const queryVO = ref({})

watchEffect(() => {
  queryVO.value = props.query
})

// 查询
function toQuery() {
  if (typeof props.toQuery === 'function') {
    props.toQuery()
  }
  emit('to-query')
}
</script>
