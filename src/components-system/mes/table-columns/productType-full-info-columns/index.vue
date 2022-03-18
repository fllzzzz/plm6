<template>
  <productType-base-info-columns
    :productType="productType"
    :category="category"
    :columns="columns"
    :fixed="fixed"
    @drawingPreview="drawingPreview"
    :snClickable="snClickable"
    :unShowField="unShowField"
    :fixedWidth="fixedWidth"
  />
  <productType-spec-info-columns
    :productType="productType"
    :category="category"
    :columns="columns"
    :unitNewLine="unitNewLine"
    :fixed="fixed"
    :unShowField="unShowField"
    :fixedWidth="fixedWidth"
  >
    <template #quantity>
      <slot name="quantity" />
    </template>
  </productType-spec-info-columns>
  <productType-other-info-columns
    :productType="productType"
    :category="category"
    :columns="columns"
    :fixed="fixed"
    :unShowField="unShowField"
    :fixedWidth="fixedWidth"
  />
</template>

<script setup>
import { defineProps, defineEmits } from 'vue'
import productTypeBaseInfoColumns from '../productType-base-info-columns'
import productTypeSpecInfoColumns from '../productType-spec-info-columns'
import productTypeOtherInfoColumns from '../productType-other-info-columns'

const emit = defineEmits(['drawingPreview'])

defineProps({
  productType: {
    type: Number
  },
  // 围护子类型
  category: {
    type: Number
  },
  columns: {
    type: Object
  },
  unitNewLine: {
    type: Boolean,
    default: true
  },
  fixed: {
    // 定位
    type: String
  },
  fixedWidth: {
    type: Boolean
  },
  // 编号可点击预览
  snClickable: {
    type: Boolean,
    default: false
  },
  unShowField: {
    type: Array,
    default: () => []
  }
})

function drawingPreview(row) {
  emit('drawingPreview', row)
}
</script>
