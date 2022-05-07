<template>
  <div>
    <bimDrawingView v-if="boolBim" v-loading="fileLoading" ref="viewBimRef" @changeFileLoading="changeFileLoading"> </bimDrawingView>
    <pdfView ref="viewPdfRef" v-loading="fileLoading" @changeFileLoading="changeFileLoading" v-if="!boolBim"></pdfView>
    <div v-if="!boolBim" class="quick-operation-small">
      <div class="icon-box" @click="fullscreen">
        <svg-icon class="icon" icon-class="fullscreen" />
      </div>
    </div>
    <drawing-preview-fullscreen-dialog
      v-model="showFullscreenDrawing"
      :bool-bim="boolBim"
      :serial-number="serialNumber"
      :productId="productId"
      :productType="productType"
    />
  </div>
</template>

<script setup>
import { defineProps, provide, ref, computed, defineExpose, nextTick } from 'vue'
import bimDrawingView from '@/components-system/bim/bim-drawing-view.vue'
import pdfView from './pdf-view'
import drawingPreviewFullscreenDialog from '@comp-base/drawing-preview/drawing-preview-fullscreen-dialog'

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

const viewBimRef = ref()
const viewPdfRef = ref()
const fileLoading = ref(false)
const showFullscreenDrawing = ref(false)

function fetch() {
  nextTick(() => {
    if (props.boolBim) {
      viewBimRef.value?.fetchDrawing()
      viewBimRef.value?.reset()
    } else {
      viewPdfRef.value?.fetchDrawing()
      viewBimRef.value?.reset()
      viewPdfRef.value?.setScale(0.5)
    }
  })
}

function changeFileLoading(state) {
  fileLoading.value = state
}

function fullscreen() {
  // viewPdfRef.value?.setScale(1)
  showFullscreenDrawing.value = true
}

defineExpose({
  fetch
})
</script>

<style lang="scss" scoped>
@import '@/styles/index.scss';
.quick-operation-small {
  position: absolute;
  left: 5px;
  top: 5px;
  width: 50px;
  height: 50px;
  @extend .flex-cac;
  .icon-box:nth-child(n) {
    position: relative;
    @extend .flex-rcc;
    width: 35px;
    height: 35px;
    :hover {
      width: 80%;
      height: 80%;
    }
  }
  .icon-box {
    cursor: pointer;
    background: rgb(83, 83, 82);
    // background: black;
    border-radius: 50%;
    opacity: 0.5;
    &:hover {
      opacity: 1;
      // background: rgb(83, 83, 82);
      // border-radius: 50%;
    }
  }
  .icon {
    position: absolute;
    width: 25px;
    height: 25px;
  }
}
</style>
