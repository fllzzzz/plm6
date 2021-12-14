<template>
  <component :is="comp" :columns="props.columns" :basic-class="props.basicClass" :showUnit="props.showUnit" :field="field" />
</template>

<script setup>
import { defineProps, computed, provide } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { isBlank } from '@/utils/data-type'
import rawMat from './module/raw-mat.vue'

const props = defineProps({
  basicClass: {
    type: Number
  },
  showUnit: {
    type: Boolean,
    default: true
  },
  columns: {
    type: Object
  },
  field: {
    // 字段
    type: String,
    default: 'material'
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

// 根据传入的物料字段获取信息
function getInfo(row, field) {
  const materialField = props.field
  if (isBlank(row) || isBlank(row[materialField])) return
  return !field ? row[materialField] : row[materialField][field]
}
provide('getInfo', getInfo)
</script>
