<template>
  <productType-base-info-columns
    :productType="productType"
    :category="category"
    :columns="columns"
    :fixed="fixed"
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
import { defineProps } from 'vue'
import productTypeBaseInfoColumns from '../productType-base-info-columns'
import productTypeSpecInfoColumns from '../productType-spec-info-columns'
import productTypeOtherInfoColumns from '../productType-other-info-columns'

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
  unShowField: {
    type: Array,
    default: () => []
  }
})
</script>
