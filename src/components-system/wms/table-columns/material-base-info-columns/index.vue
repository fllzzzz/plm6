<template>
  <component :is="comp" :columns="props.columns" :basic-class="props.basicClass" :spec-merge="props.specMerge" :show-factory="props.showFactory" />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import steelPlate from './module/steel-plate.vue'
import sectionSteel from './module/section-steel.vue'
import steelCoil from './module/steel-coil.vue'
import auxMat from './module/aux-mat.vue'
import gas from './module/gas.vue'
import rawMat from './module/raw-mat.vue'

const props = defineProps({
  specMerge: { // 规格合并,规格与 厚宽长颜色等合并为一个字段
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  },
  columns: {
    type: Object
  },
  showFactory: {
    type: Boolean,
    default: false
  }
})

const comp = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V: return steelPlate
    case rawMatClsEnum.SECTION_STEEL.V: return sectionSteel
    case rawMatClsEnum.STEEL_COIL.V: return steelCoil
    case rawMatClsEnum.MATERIAL.V: return auxMat
    case rawMatClsEnum.GAS.V: return gas
    default: return rawMat
  }
})

</script>
