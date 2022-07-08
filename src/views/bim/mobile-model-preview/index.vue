<template>
  <bim-model-view
    ref="mobileModelPreview"
    :monomer-id="monomerId"
    :serial-number="serialNumber"
    :productId="productId"
    :productType="productType"
    :preview-show-all="Boolean(showAll)"
    is-preview
    class="mobile-model-preview"
  ></bim-model-view>
  <el-radio-group class="mobile-model-preview-radio" v-if="!Boolean(showAll) && loaded" v-model="showContent" @change="contentChange">
    <el-radio-button :label="false">显示构件</el-radio-button>
    <el-radio-button :label="true">显示全部</el-radio-button>
  </el-radio-group>
</template>

<script setup>
import { ref, computed } from 'vue'
import { useRoute } from 'vue-router'
import bimModelView from '@/components-system/bim/bim-model-view'
import { ElRadioGroup } from 'element-plus'

const route = useRoute()
const productId = route.query.productId && Number(route.query.productId)
const productType = route.query.productType && Number(route.query.productType)
const showAll = (route.query.showAll && Number(route.query.showAll)) || 0 // 0:不显示全部，1：显示全部
const serialNumber = route.query.serialNumber
const monomerId = route.query.monomerId && Number(route.query.monomerId)

const mobileModelPreview = ref()
const showContent = ref(false)
const loaded = computed(() => mobileModelPreview.value?.modelLoaded)

function contentChange() {
  if (showContent.value) {
    mobileModelPreview.value?.clearIsolation()
  } else {
    const _ids = mobileModelPreview.value?.previewSNElementIds.value
    if (!_ids || !_ids?.length) return
    mobileModelPreview.value?.isolateComponentsById(_ids)
  }
}
</script>

<style lang="scss">
.mobile-model-preview {
  .bf-toolbar.bf-toolbar-bottom {
    position: absolute;
    left: 10px;
    top: 10px;
    bottom: unset;
    right: unset;
    transform: none;
  }
}

.mobile-model-preview-radio {
  position: absolute;
  top: 20px;
  left: 125px;
}
</style>
