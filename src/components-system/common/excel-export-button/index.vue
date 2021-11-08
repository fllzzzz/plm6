<template>
  <common-button :loading="exportLoading" :size="size" :disabled="disabled" icon="el-icon-download" :type="btnType" @click="download" >
    {{props.btnName}}
  </common-button>
</template>

<script setup>
import { defineEmits, defineProps, ref } from 'vue'
import downloadXLSX from '@/utils/print/download'
import { ElNotification } from 'element-plus'

const emit = defineEmits(['end'])

const props = defineProps({
  beforeDownload: { // 打印前调用的方法
    type: Function,
    default: undefined
  },
  header: {
    type: Object,
    default: () => {}
  },
  template: {
    type: Object,
    required: true
  },
  title: {
    type: String
  },
  filename: {
    type: String
  },
  params: {
    type: null
  },
  size: {
    type: String,
    default: 'small'
  },
  disabled: {
    type: Boolean,
    default: false
  },
  btnName: {
    type: String,
    default: '下载'
  },
  btnType: {
    type: String
  }
})

const exportLoading = ref(false)

async function fetch(params) {
  try {
    const data = await props.template.fetch(params) || {}
    return data
  } catch (error) {
    throw new Error('加载下载数据失败')
  }
}

async function download() {
  // beforeDownload 返回 false 停止打印
  if (typeof props.beforeDownload !== 'function' || props.beforeDownload() !== false) {
    exportLoading.value = true
    try {
      // 传入数组批量导出
      let params = props.params
      if (!(props.params instanceof Array)) {
        params = [props.params]
      }
      for (const p of params) {
        const { header, footer, table, qrCode } = await fetch(p) || {}
        const result = await downloadXLSX({
          title: props.title,
          filename: props.filename,
          header, footer, table, qrCode,
          config: props.template
        })
        if (!result) {
          throw new Error('导出失败')
        }
      }
      emit('end')
    } catch (error) {
      ElNotification({ title: '导出失败', message: error, type: 'error', duration: 2500 })
      console.log('导出', error)
    } finally {
      exportLoading.value = false
    }
  }
}
</script>
