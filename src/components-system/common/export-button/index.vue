<template>
  <common-button
    :loading="downloadLoading"
    :disabled="props.disabled"
    :type="props.type"
    :icon="props.icon"
    :size="props.size"
    @click.stop="doExport"
  >
    <span v-if="props.btnText">
      {{ props.btnText }}
    </span>
  </common-button>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { fileDownload } from '@/utils/file'
import { downloadAttachment } from '@/api/common'

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
  btnText: {
    type: String,
    default: ''
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
  }
})

const downloadLoading = ref(false)

async function doExport(params) {
  try {
    downloadLoading.value = true
    await fileDownload(props.fn, props.params)
  } catch (error) {
    console.log('通用导出', error)
  } finally {
    downloadLoading.value = false
  }
}
</script>
