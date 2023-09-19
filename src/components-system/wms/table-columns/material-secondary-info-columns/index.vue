<template>
  <component :is="comp" :basicClass="basicClass" :columns="columns" :show-batch-no="showBatchNo" :fixed="fixed" />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import steel from './module/steel.vue'
import rawMat from './module/raw-mat.vue'
import manufMat from './module/manuf-mat.vue'
import { STEEL_ENUM, MANUF_ENUM } from '@/settings/config'

const props = defineProps({
  basicClass: {
    type: Number
  },
  columns: {
    type: Object
  },
  showBatchNo: {
    // 显示炉批号
    type: Boolean,
    default: true
  },
  fixed: {
    // 定位
    type: String
  }
})

const comp = computed(() => {
  if (props.basicClass & STEEL_ENUM) {
    return steel
  }
  if (props.basicClass & MANUF_ENUM) {
    return manufMat
  }
  switch (props.basicClass) {
    case matClsEnum.STEEL_PLATE.V:
    case matClsEnum.SECTION_STEEL.V:
    case matClsEnum.STEEL_COIL.V:
      return steel
    case matClsEnum.STRUC_MANUFACTURED.V:
    case matClsEnum.ENCL_MANUFACTURED.V:
      return manufMat
    case matClsEnum.MATERIAL.V:
    case matClsEnum.GAS.V:
    default:
      return rawMat
  }
})
</script>
