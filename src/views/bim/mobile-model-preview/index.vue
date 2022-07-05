<template>
  <bim-model-view
    :monomer-id="monomerId"
    :serial-number="serialNumber"
    :productId="productId"
    :productType="productType"
    :preview-show-all="Boolean(showAll)"
    is-preview
  ></bim-model-view>
</template>

<script setup>
import { useRoute } from 'vue-router'
import bimModelView from '@/components-system/bim/bim-model-view'

const route = useRoute()
const productId = route.query.productId && Number(route.query.productId)
const productType = route.query.productType && Number(route.query.productType)
const showAll = route.query.showAll && Number(route.query.showAll) || 0 // 0:不显示全部，1：显示全部
const serialNumber = route.query.serialNumber
const monomerId = route.query.monomerId && Number(route.query.monomerId)

</script>

<style lang="scss" scoped></style>
