<template>
  <el-tooltip class="item" effect="dark" :content="props.tip" :disabled="!props.showTooltip" placement="top-start">
    <common-button
      v-permission="props.permission"
      :loading="downloadLoading"
      :disabled="props.disabled"
      type="warning"
      icon="el-icon-download"
      size="mini"
      @click.stop="doExport"
    />
  </el-tooltip>
</template>

<script setup>
import { computed, defineProps, ref } from 'vue'
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

// 获取数据源
const currentData = computed(() => {
  const data = props.data
  if (data) {
    if (data.sourceRow) {
      return data.sourceRow
    } else {
      return data
    }
  }
  return data
})

async function doExport() {
  try {
    downloadLoading.value = true
    if (props.customFn) {
      await fileDownload(props.customFn, currentData.value)
    } else {
      crud.downloadLoading = true
      await crud.doExport(currentData.value)
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
