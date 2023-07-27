<template>
  <common-button
    :loading="downloadLoading"
    :disabled="props.disabled"
    :type="props.type"
    :icon="props.icon"
    :size="props.size"
    @click.stop="doExport"
  >
    <slot />
  </common-button>
</template>

<script setup>
import { defineProps, ref, defineEmits } from 'vue'
import { fileDownload, fileDownloadByReturnStatus } from '@/utils/file'
import { downloadAttachment } from '@/api/common'

const emit = defineEmits(['download-success'])

const props = defineProps({
  params: {
    type: null,
    default: undefined
  },
  disabled: {
    type: Boolean,
    default: false
  },
  fn: {
    type: Function,
    default: downloadAttachment
  },
  size: {
    type: String,
    default: 'mini'
  },
  type: {
    type: String,
    default: 'warning'
  },
  icon: {
    type: String,
    default: 'el-icon-download'
  },
  // responseHeader 中 result 为 code,message的情景下使用
  responseHeaderResult: {
    type: Boolean,
    default: false
  }
})

const downloadLoading = ref(false)

async function doExport(params) {
  try {
    downloadLoading.value = true
    if (props.responseHeaderResult) {
      await fileDownloadByReturnStatus(props.fn, props.params)
    } else {
      await fileDownload(props.fn, props.params)
    }
    emit('download-success')
  } catch (error) {
    console.log('通用导出', error)
  } finally {
    downloadLoading.value = false
  }
}
</script>
