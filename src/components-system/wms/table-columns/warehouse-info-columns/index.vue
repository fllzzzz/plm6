<template>
  <component v-bind="$attrs" :is="comp" :columns="columns" :show-project="showProject" :show-transfer="showTransfer" :show-monomer="showMonomer" :show-area="showArea" :show-workshop="showWorkshop" />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import rawMat from './module/raw-mat.vue'

const props = defineProps({
  basicClass: {
    type: Number
  },
  showProject: { // 显示项目
    type: Boolean,
    default: false
  },
  showTransfer: { // 项目标签 显示调拨
    type: Boolean,
    default: false
  },
  showMonomer: { // 显示单体
    type: Boolean,
    default: false
  },
  showArea: { // 显示区域
    type: Boolean,
    default: false
  },
  showWorkshop: { // 显示车间
    type: Boolean,
    default: false
  },
  columns: { // 用于crud组件的列显隐
    type: Object
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
