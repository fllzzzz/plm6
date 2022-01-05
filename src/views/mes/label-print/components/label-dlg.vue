<template>
  <common-dialog
    :title="`${component.name}-${component.serialNumber}`"
    v-model:visible="dialogVisible"
    width="640px"
    :before-close="handleClose"
  >
    <div style="position: relative">
      <span v-html="showInfo.showHtml"> </span>
      <qrcode-vue
        :value="labelData.qrCode"
        :size="showInfo?.qrPosition?.size"
        :margin="2"
        :style="`position: absolute;
        right: ${showInfo?.qrPosition?.right};
        left: ${showInfo?.qrPosition?.left};
        top: ${showInfo?.qrPosition?.top};
        bottom: ${showInfo?.qrPosition?.bottom};`"
      />
    </div>
  </common-dialog>
</template>

<script setup>
import { computed, defineEmits, defineProps } from 'vue'
import QrcodeVue from 'qrcode.vue'

import { getPreviewLabelHtml } from '@/utils/label/index.js'

import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  labelData: {
    type: Object,
    default: () => {}
  },
  productType: {
    type: Number
  },
  labelType: {
    type: Number
  }
})

const component = computed(() => props.labelData.component || {})
const showInfo = computed(() => {
  return getPreviewLabelHtml({
    productType: props.productType,
    labelType: props.labelType,
    labelData: props.labelData
  })
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
</script>
