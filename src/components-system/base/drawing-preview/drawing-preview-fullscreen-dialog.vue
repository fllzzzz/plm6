<template>
  <el-dialog
    title="图纸预览"
    v-model="dialogVisible"
    width="80%"
    :fullscreen="true"
    :custom-class="'drawing-pdf-dialog'"
    :before-close="handleClose"
  >
    <div v-show="!fileLoading" class="root-content">
      <div class="preview-content" @dblclick="changeOperate(true)" @click="changeOperate(false)">
        <component ref="viewRef" :is="currentView" @changeFileLoading="changeFileLoading"> </component>
      </div>
      <div v-show="showOperate" class="operate-content">
        <div class="operate-left">{{ serialNumber }}</div>
        <div v-if="!boolBim" class="operate-middle">{{ `${pageNum} / ${pageTotalNum}` }}</div>
        <div v-else class="operate-middle"></div>
        <div class="operate-right" />
      </div>
      <div class="quick-operation">
        <div class="icon-box" @click="scaleZoom">
          <svg-icon class="icon" icon-class="comp-zoom" />
        </div>
        <div class="icon-box" @click="scaleZoomOut">
          <svg-icon class="icon" icon-class="comp-zoom-out" />
        </div>
        <div v-if="!boolBim" class="icon-box" @click="clockwiseRotate">
          <svg-icon class="icon" icon-class="comp-clockwise-rotate" />
        </div>
        <div v-if="!boolBim" class="icon-box" @click="counterclockwiseRotate">
          <svg-icon class="icon" icon-class="comp-counterclockwise-rotate" />
        </div>
        <div class="icon-box" @click="reset">
          <svg-icon class="icon" icon-class="comp-restore-size" />
        </div>
        <div class="icon-box" @click="handleClose">
          <svg-icon class="icon" icon-class="comp-quit" />
        </div>
      </div>
    </div>
    <div v-if="fileLoading" class="load-box">
      <i class="el-icon-loading" />
    </div>
  </el-dialog>
</template>

<script setup>
import { defineEmits, defineProps, provide, ref, computed, nextTick } from 'vue'
import useVisible from '@compos/use-visible'
import bimDrawingView from '@/components-system/bim/bim-drawing-view.vue'
import pdfView from './pdf-view'
// bim内手动挂载组件需引入涉及组件
import { ElDialog } from 'element-plus'
import SvgIcon from '@comp/SvgIcon/index.vue'

const emit = defineEmits(['update:modelValue'])
const props = defineProps({
  modelValue: {
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

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'modelValue', showHook: initPreview })

const viewRef = ref()
const showOperate = ref(false)
const pageNum = ref()
const pageTotalNum = ref()
const fileLoading = ref(false)

const currentView = computed(() => {
  console.log(props.boolBim, 'props.boolBim')
  return props.boolBim ? bimDrawingView : pdfView
})

function initPreview() {
  if (!dialogVisible.value) {
    return
  }
  pageNum.value = 1
  pageTotalNum.value = 1
  nextTick(() => {
    viewRef.value.fetchDrawing()
    viewRef.value.reset()
  })
}

function changeOperate(state) {
  showOperate.value = state
}

function changeFileLoading(state) {
  fileLoading.value = state
}

function scaleZoom() {
  viewRef.value.scaleZoom()
}
function scaleZoomOut() {
  viewRef.value.scaleZoomOut()
}
function clockwiseRotate() {
  viewRef.value.clockwiseRotate()
}
function counterclockwiseRotate() {
  viewRef.value.counterclockwiseRotate()
}
function reset() {
  viewRef.value.reset()
}
</script>

<style lang="scss">
@import '@/styles/index.scss';

.drawing-pdf-dialog {
  background: #525659;
  .el-dialog__header {
    display: none;
  }
  .el-dialog__body {
    padding: 0;
  }
  .root-content {
    position: relative;
  }
  .load-box {
    @extend .flex-rcc;
    position: absolute;
    top: 0;
    right: 0;
    width: 100%;
    height: 100%;
    font-size: 100px;
    color: white;
  }
  .preview-content {
    box-sizing: border-box;
    padding: 0 50px;
    @extend .flex-rcc;
    width: 100%;
    height: 100vh;
  }
  .pdf-box {
    width: 100%;
    height: 100vh;
    overflow: auto;
    span {
      width: 100%;
      margin: 0 auto;
    }
  }
}

.quick-operation {
  position: absolute;
  bottom: 15vh;
  right: 0vh;
  width: 50px;
  height: 450px;
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

.operate-content {
  position: absolute;
  top: 0;
  z-index: 1;
  height: 50px;
  width: 100%;
  box-sizing: border-box;
  padding: 0 100px;
  background: #323639;
  color: white;
  @extend .flex-rbc;
  .operate-middle {
    @extend .flex-rsc;
  }
  .operate-right {
    @extend .flex-rsc;
    .icon-box:nth-child(n) {
      position: relative;
      @extend .flex-rcc;
      width: 35px;
      height: 35px;
      margin-left: 15px;
      :hover {
        width: 80%;
        height: 80%;
      }
    }
    .icon-box {
      cursor: pointer;
      &:hover {
        background: rgb(83, 83, 82);
        border-radius: 50%;
      }
    }
  }
  // .icon {
  //     position: absolute;
  //     width: 25px;
  //     height: 25px;
  // }
}
</style>
