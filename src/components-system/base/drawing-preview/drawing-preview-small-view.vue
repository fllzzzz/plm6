<template>
  <component v-loading="fileLoading" ref="viewRef" :is="currentView" @changeFileLoading="changeFileLoading"> </component>
</template>

<script setup>
import { defineProps, provide, ref, computed, watch, nextTick } from 'vue'
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
provide('boolBim', computed(() => props.boolBim))

const viewRef = ref()
const fileLoading = ref(false)

const currentView = computed(() => {
  return bimDrawingView
})

watch(
  [() => props.serialNumber, () => props.productId, () => props.productType],
  () => {
    nextTick(() => {
      viewRef?.value?.fetchDrawing()
      viewRef?.value?.reset()
    })
  },
  { immediate: true }
)

function changeFileLoading(state) {
  fileLoading.value = state
}
</script>

<style lang="scss" scoped></style>
