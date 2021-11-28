<template>
  <component :is="comp" />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import steel from './module/steel.vue'

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
    case rawMatClsEnum.MATERIAL.V: return steel
    case rawMatClsEnum.GAS.V: return steel
    default: return steel
  }
})

</script>
