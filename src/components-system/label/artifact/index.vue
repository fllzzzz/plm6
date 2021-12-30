<template>
  <div style="position: relative">
    <span v-html="showHtml"> </span>
    <qrcode-vue :value="labelData.qrCode" :size="160" :margin="2" style="position: absolute; right: 18px;bottom: 8px;"/>
  </div>
</template>

<script setup>
import QrcodeVue from 'qrcode.vue'

import { computed, defineProps } from 'vue'
import { getPreviewLabelHtml } from '../label-fn'

const props = defineProps({
  labelData: {
    type: Object,
    default: () => {}
  },
  labelType: {
    type: Number
  },
  productType: {
    type: Number
  }
})

const showHtml = computed(() => {
  return getPreviewLabelHtml({
    productType: props.productType,
    labelType: props.labelType,
    labelData: props.labelData
  })
})
</script>
