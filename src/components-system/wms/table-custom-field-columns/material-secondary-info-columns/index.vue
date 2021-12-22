<template>
  <component :is="comp" :basicClass="basicClass" :columns="columns" :show-batch-no="showBatchNo" :field="field" :fixed="fixed" />
</template>

<script setup>
import { defineProps, computed, provide } from 'vue'
import { STEEL_ENUM } from '@/settings/config'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { isBlank } from '@/utils/data-type'
import steel from './module/steel.vue'
import rawMat from './module/raw-mat.vue'

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
  field: {
    // 字段
    type: String,
    default: 'material'
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
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
    case rawMatClsEnum.SECTION_STEEL.V:
    case rawMatClsEnum.STEEL_COIL.V:
      return steel
    case rawMatClsEnum.MATERIAL.V:
    case rawMatClsEnum.GAS.V:
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
