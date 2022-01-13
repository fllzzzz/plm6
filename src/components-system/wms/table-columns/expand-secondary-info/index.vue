<template>
  <component :is="comp" :row="row" :show-remark="showRemark" :show-brand="showBrand">
    <slot />
  </component>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import steelPlate from './module/steel-plate.vue'
import sectionSteel from './module/section-steel.vue'
import steelCoil from './module/steel-coil.vue'
import auxMat from './module/aux-mat.vue'
import gas from './module/gas.vue'

const props = defineProps({
  basicClass: {
    // 基础分类
    type: Number
  },
  row: {
    // 当前行数据
    type: Object,
    default: () => {
      return {}
    }
  },
  showBrand: {
    type: Boolean,
    default: false
  },
  showRemark: {
    // 显示备注
    type: Boolean,
    default: false
  }
})

const comp = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return steelPlate
    case rawMatClsEnum.SECTION_STEEL.V:
      return sectionSteel
    case rawMatClsEnum.STEEL_COIL.V:
      return steelCoil
    case rawMatClsEnum.MATERIAL.V:
      return auxMat
    case rawMatClsEnum.GAS.V:
      return gas
    default:
      return auxMat
  }
})
</script>
