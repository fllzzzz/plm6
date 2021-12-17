<template>
  <component :is="comp" :columns="columns" :show-invoice-type="showInvoiceType" :show-tax-rate="showTaxRate" />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import rawMat from './module/raw-mat.vue'

const props = defineProps({
  basicClass: {
    type: Number
  },
  columns: { // 用于crud组件的列显隐
    type: Object
  },
  // 显示票据类型
  showInvoiceType: {
    type: Boolean,
    default: false
  },
  // 显示税率
  showTaxRate: {
    type: Boolean,
    default: false
  }
})

const comp = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
    case rawMatClsEnum.SECTION_STEEL.V:
    case rawMatClsEnum.STEEL_COIL.V:
    case rawMatClsEnum.MATERIAL.V:
    case rawMatClsEnum.GAS.V:
      return rawMat
    default:
      return rawMat
  }
})
</script>
