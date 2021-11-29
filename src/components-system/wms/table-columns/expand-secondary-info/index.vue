<template>
  <component :is="comp" :row="row" :showRemark="showRemark" />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import steel from './module/steel.vue'

const props = defineProps({
  basicClass: { // 基础分类
    type: Number
  },
  row: { // 当前行数据
    type: Object,
    default: () => {
      return {}
    }
  },
  showRemark: { // 显示备注
    type: Boolean,
    default: false
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
