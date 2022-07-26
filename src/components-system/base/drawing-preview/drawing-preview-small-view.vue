<template>
  <div>
    <bimDrawingView
      v-if="boolBim"
      v-loading="fileLoading"
      ref="viewBimRef"
      @changeFileLoading="changeFileLoading"
      style="overflow: auto; height: 100%"
    >
    </bimDrawingView>
    <pdfView
      ref="viewPdfRef"
      v-loading="fileLoading"
      @changeFileLoading="changeFileLoading"
      v-if="!boolBim"
      style="overflow: auto; height: 100%"
    ></pdfView>
    <div v-if="!boolBim" class="quick-operation-small">
      <div class="icon-box" @click="fullscreen">
        <svg-icon class="icon" icon-class="fullscreen" />
      </div>
      <div v-if="multipleDrawing" class="operate-left">
        <el-radio-group v-model="curDSN" size="mini">
          <el-radio-button v-for="item in drawingSN" :key="item" :label="item">
            {{ `${serialNumber}${item ? '_' + item : ''}` }}
          </el-radio-button>
        </el-radio-group>
      </div>
    </div>
    <drawing-preview-fullscreen-dialog
      v-model="showFullscreenDrawing"
      :bool-bim="boolBim"
      :drawingSN="drawingSN"
      :cur-drawing-s-n="curDSN"
      :serial-number="serialNumber"
      :productId="productId"
      :productType="productType"
    />
  </div>
</template>

<script setup>
import { defineProps, provide, ref, computed, defineExpose, nextTick, watch } from 'vue'
import { isBlank } from '@data-type/index'
import bimDrawingView from '@/components-system/bim/bim-drawing-view.vue'
import pdfView from './pdf-view'
import { ElRadioGroup } from 'element-plus'
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
  drawingSN: {
    type: [String, Array]
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
const curDSN = ref()

const multipleDrawing = computed(() => Array.isArray(props.drawingSN) && props.drawingSN?.length)

provide('multipleDrawing', multipleDrawing)
provide('drawingSN', curDSN)

watch(
  () => curDSN.value,
  () => {
    if (isBlank(curDSN.value)) return
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
)

function closeHandle() {
  curDSN.value = undefined
}

function fetch() {
  if (multipleDrawing.value) {
    curDSN.value = props.drawingSN[0]
  } else {
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
}

function changeFileLoading(state) {
  fileLoading.value = state
}

function fullscreen() {
  // viewPdfRef.value?.setScale(1)
  showFullscreenDrawing.value = true
}

defineExpose({
  fetch,
  closeHandle
})
</script>

<style lang="scss" scoped>
@import '@/styles/index.scss';
.quick-operation-small {
  position: absolute;
  left: 0px;
  top: 5px;
  padding: 0 10px;
  width: 100%;
  height: 50px;
  @extend .flex-rsc;
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
