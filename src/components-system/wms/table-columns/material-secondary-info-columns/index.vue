<template>
  <component :is="comp" />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import steel from './module/steel.vue'
import auxMat from './module/aux-mat.vue'
import gas from './module/gas.vue'

const props = defineProps({
  basicClass: {
    type: Number
  }
})

const comp = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
    case rawMatClsEnum.SECTION_STEEL.V:
    case rawMatClsEnum.STEEL_COIL.V: return steel
    case rawMatClsEnum.MATERIAL.V: return auxMat
    case rawMatClsEnum.GAS.V: return gas
    default: return steel
  }
})

</script>
