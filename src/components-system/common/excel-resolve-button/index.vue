<template>
  <el-upload
    ref="uploadRef"
    class="upload-excel-resolve"
    action=""
    :before-upload="handleBefore"
    :on-change="handleChange"
    :on-exceed="handleExceed"
    :disabled="props.disabled"
    :limit="props.limit"
    :accept="props.accept"
    :auto-upload="false"
    :show-file-list="false"
  >
    <common-button
      :loading="props.resolveLoading"
      :disabled="props.disabled"
      :icon="props.icon"
      :size="props.btnSize"
      :type="props.btnType"
      >{{ props.btnName }}</common-button
    >
  </el-upload>
</template>
<script setup>
import { defineEmits, defineProps, ref } from 'vue'
import { ElUpload, ElMessage, ElLoading } from 'element-plus'
import { resolveExcel, fileVerification, formatExcelData } from '@/utils/file'

const emit = defineEmits(['data', 'success'])

const props = defineProps({
  template: {
    type: Object
  },
  // 打开 loading
  openLoading: {
    type: Boolean,
    default: false
  },
  accept: {
    type: String,
    default: '.xls,.xlsx'
  },
  successMessage: {
    type: String,
    default: '上传成功'
  },
  size: {
    type: String,
    default: 'mini'
  },
  icon: {
    type: String,
    default: 'el-icon-upload'
  },
  btnName: {
    type: String,
    default: ''
  },
  btnType: {
    type: String,
    default: 'primary'
  },
  btnSize: {
    type: String,
    default: 'small'
  },
  tip: {
    type: String,
    default: ''
  },
  disabled: {
    type: Boolean,
    default: false
  },
  limit: {
    type: Number,
    default: undefined
  },
  sizeLimit: {
    type: Number,
    default: undefined
  }
})

let loading
const uploadRef = ref()
const resolveLoading = ref(false)

// 处理前
function handleBefore(file) {
  return fileVerification(file, {
    accept: props.accept,
    sizeLimit: props.sizeLimit
  })
}

async function handleChange(file, fileList) {
  try {
    if (props.openLoading) {
      loading = ElLoading.service({
        lock: true,
        text: '正在导入数据，请稍后',
        fullscreen: true,
        background: 'rgba(255, 255, 255, 0.5)'
      })
    }
    resolveLoading.value = true
    // TODO: 清空无效
    uploadRef.value.clearFiles()
    let data = await resolveExcel(file.raw)
    if (typeof props.template === 'object') {
      data = await formatExcelData(data, props.template)
    }
    emit('data', data)
    emit('success', data)
  } catch (error) {
    ElMessage.error({ message: error.message, duration: 5000 })
  } finally {
    resolveLoading.value = false
    loading && loading.close()
  }
}

function handleExceed(files, fileList) {
  ElMessage.warning(
    `当前限制选择 ${props.limit} 个文件，本次选择了 ${files.length} 个文件，共选择了 ${files.length + fileList.length} 个文件`
  )
}
</script>

<style lang="scss" scoped>
.upload-excel-resolve {
  display: inline-block;
}
</style>
