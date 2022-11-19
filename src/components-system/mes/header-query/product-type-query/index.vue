<template>
  <component :is="currentView" :query="query" :product-type="productType" :category="category" @to-query="toQuery" :showMaterial="showMaterial" />
</template>

<script setup>
import { defineEmits, defineProps, computed, watchEffect, ref } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'

import artifact from './module/artifact'
import assemble from './module/assemble'
import enclosure from './module/enclosure'
import machinePart from './module/machine-part'

const emit = defineEmits(['to-query'])

const props = defineProps({
  productType: {
    type: Number
  },
  // 围护子类型
  category: {
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
