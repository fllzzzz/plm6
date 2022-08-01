<template>
  <drawing-preview-fullscreen-dialog
    v-model="showFullscreenDrawing"
    :bool-bim="boolBim"
    :serial-number="serialNumber"
    :productId="productId"
    :productType="productType"
    :drawingSN="drawingSN"
  />
</template>

<script setup>
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'
import { defineProps, ref, watch } from 'vue'

const showFullscreenDrawing = ref(false)

const props = defineProps({
  handleClose: {
    type: Function
  },
  showDraw: {
    type: Boolean,
    default: false
  },
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
  },
  drawingSN: {
    type: Number,
    default: undefined
  }
})

watch(
  () => props.showDraw,
  (val) => {
    if (val) {
      showFullscreenDrawing.value = true
    }
  }
)

watch(
  () => showFullscreenDrawing.value,
  (val) => {
    if (!val) {
      if (typeof props.handleClose === 'function') {
        props.handleClose()
      }
    }
  }
)
</script>
