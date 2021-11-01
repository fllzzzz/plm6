<template>
  <el-tooltip class="item" effect="dark" :content="props.tip" :disabled="!props.showTooltip" placement="top-start">
    <el-button
      v-permission="props.permission"
      :loading="downloadLoading"
      :disabled="props.disabled"
      type="warning"
      icon="el-icon-download"
      size="mini"
      @click.stop="doExport"
    >
      <span v-if="props.showText" v-text="tip" />
    </el-button>
  </el-tooltip>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { regExtra } from '@compos/use-crud'
import { isNotBlank } from '@data-type/index'
import { fileDownload } from '@/utils/file'

const props = defineProps({
  data: {
    type: null,
    default: undefined
  },
  permission: {
    type: Array,
    required: true
  },
  disabled: {
    type: Boolean,
    default: false
  },
  tip: {
    type: String,
    default: '导出'
  },
  customFn: {
    type: Function,
    default: undefined
  },
  showText: {
    type: Boolean,
    default: false
  },
  showTooltip: {
    type: Boolean,
    default: false
  }
})

const downloadLoading = ref(false)
const { crud } = regExtra()

async function doExport() {
  try {
    downloadLoading.value = true
    if (props.customFn) {
      await fileDownload(props.customFn, props.data)
    } else {
      crud.downloadLoading = true
      await crud.doExport(props.data)
    }
  } catch (error) {
    console.log('通用导出', error)
  } finally {
    downloadLoading.value = false
    if (isNotBlank(props.customFn)) {
      crud.downloadLoading = false
    }
  }
}
</script>
