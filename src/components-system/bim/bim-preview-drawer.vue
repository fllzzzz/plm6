<template>
  <common-drawer
    ref="drawerRef"
    title=""
    v-model="drawerVisible"
    direction="btt"
    :with-header="false"
    :before-close="handleClose"
    size="50%"
    :custom-class="'model-drawer'"
  >
    <template #titleRight> </template>
    <template #content>
      <div class="model-drawer-container">
        <bim-model-view
          :monomer-id="monomerId"
          :serial-number="serialNumber"
          :productId="productId"
          :productType="productType"
          :boolBim="boolBim"
          is-preview
          class="model"
          :max-height="maxHeight"
        ></bim-model-view>
        <drawing-preview-small-view
          ref="drawingRef"
          :boolBim="boolBim"
          :serial-number="serialNumber"
          :productId="productId"
          :productType="productType"
          class="drawing"
          :style="{ height: `${maxHeight}px`,overflow: 'auto' }"
        ></drawing-preview-small-view>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref, computed, nextTick } from 'vue'

import useVisible from '@compos/use-visible'
import bimModelView from '@/components-system/bim/bim-model-view'
import drawingPreviewSmallView from '@comp-base/drawing-preview/drawing-preview-small-view'

const drawerRef = ref()
const drawingRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  boolBim: {
    type: Boolean,
    default: false
  },
  monomerId: {
    type: Number,
    default: undefined
  },
  serialNumber: {
    type: String
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

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: show })

// 高度
const maxHeight = computed(() => document.documentElement.clientHeight * 0.5)

function show() {
  nextTick(() => {
    drawingRef.value?.fetch()
  })
}
</script>

<style lang="scss" scoped>
.model-drawer-container {
  display: flex;

  .model {
    flex: 1;
  }

  .drawing{
    width: 40%;
  }
}
</style>

<style lang="scss">
.model-drawer {
  .el-drawer__body {
    padding: 0px;
  }

  .bf-toolbar.bf-toolbar-bottom {
    position: absolute;
    left: 10px;
    top: 10px;
    bottom: unset;
    right: unset;
    transform: none;
  }
}
</style>
