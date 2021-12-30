<template>
  <common-dialog
    :title="`${component.name}-${component.serialNumber}`"
    v-model:visible="dialogVisible"
    width="640px"
    :before-close="handleClose"
  >
    <component :is="currentView" :labelType="labelType" :labelData="labelData" :productType="productType"/>
  </common-dialog>
</template>

<script setup>
import { computed, defineEmits, defineProps } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'

import useVisible from '@compos/use-visible'
import artifactLabel from '@comp-label/artifact'

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

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

const currentView = computed(() => {
  switch (props.productType) {
    case componentTypeEnum.ARTIFACT.V:
      return artifactLabel
    case componentTypeEnum.ENCLOSURE.V:
      return artifactLabel
    default:
      return artifactLabel
  }
})
</script>
