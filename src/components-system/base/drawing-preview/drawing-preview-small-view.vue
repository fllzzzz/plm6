<template>
  <bimDrawingView v-loading="fileLoading" ref="viewRef" @changeFileLoading="changeFileLoading"> </bimDrawingView>
</template>

<script setup>
import { defineProps, provide, ref, computed, defineExpose, nextTick } from 'vue'
import bimDrawingView from '@/components-system/bim/bim-drawing-view.vue'
// import pdfView from './pdf-view'

const props = defineProps({
  boolBim: {
    type: Boolean,
    default: false
  },
  serialNumber: {
    // 编号
    type: String,
    default: undefined
  },
  productId: {
    type: Number,
    default: undefined
  },
  productType: {
    type: Number,
    default: undefined
  }
})

provide(
  'productId',
  computed(() => props.productId)
)
provide(
  'productType',
  computed(() => props.productType)
)
provide('isPreview', true)
provide('id', 'small')
provide(
  'boolBim',
  computed(() => props.boolBim)
)

const viewRef = ref()
const fileLoading = ref(false)

function fetch() {
  nextTick(() => {
    viewRef.value?.fetchDrawing()
    viewRef.value?.reset()
  })
}

function changeFileLoading(state) {
  fileLoading.value = state
}

defineExpose({
  fetch
})
</script>

<style lang="scss" scoped></style>
