<template>
  <component :is="comp" :list="list" :maxHeight="maxHeight"/>
</template>

<script setup>
import { computed, defineProps } from 'vue'
import { materialPurchaseClsEnum } from '@enum-ms/classification'

import ManufTable from './module/manufactured.vue'
import RawMaterialTable from './module/raw-material.vue'

const props = defineProps({
  materialType: {
    type: Number
  },
  list: {
    type: Array,
    default: () => []
  },
  maxHeight: {
    type: Number
  }
})

const comp = computed(() => {
  switch (props.materialType) {
    case materialPurchaseClsEnum.MANUFACTURED.V:
      return ManufTable
    case materialPurchaseClsEnum.STEEL.V:
    case materialPurchaseClsEnum.MATERIAL.V:
      return RawMaterialTable
    default:
      return undefined
  }
})
</script>

<style lang="scss" scoped></style>
