<template>
  <component :is="currentView" :query="query" :product-type="productType" @to-query="toQuery" :showMaterial="showMaterial" />
</template>

<script setup>
import { defineEmits, defineProps, computed, watchEffect, ref } from 'vue'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'

import box from './module/box'
import element from './module/element'
import machinePart from './module/machine-part'

const emit = defineEmits(['to-query'])

const props = defineProps({
  productType: {
    type: Number
  },
  query: {
    type: Object,
    default: () => {
      return {}
    }
  },
  toQuery: {
    type: Function
  },
  showMaterial: {
    type: Boolean,
    default: true
  }
})

const currentView = computed(() => {
  switch (props.productType) {
    case bridgeComponentTypeEnum .BOX.V:
      return box
    case bridgeComponentTypeEnum .MACHINE_PART.V:
      return machinePart
    case bridgeComponentTypeEnum .AUXILIARY_MATERIAL.V:
      return ''
    case bridgeComponentTypeEnum .CELL.V:
      return element
    default:
      return ''
  }
})

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
