<template>
  <el-upload
    ref="upload"
    :data="{...props.data, fileType: props.fileClassify}"
    :action="props.action"
    :headers="headers"
    :http-request="handleRequest"
    :before-upload="handleBefore"
    :on-success="handleSuccess"
    :on-progress="handleProgress"
    :on-error="handleError"
    :limit="props.limit"
    :multiple="props.multiple"
    :on-exceed="handleExceed"
    :file-list="fileList"
    :show-file-list="false"
    :accept="props.accept"
    :disabled="props.disabled"
  >
    <common-button :loading="uploadLoading" :disabled="props.disabled" :icon="props.icon" :size="props.btnSize" :type="props.btnType">{{ props.btnName }}</common-button>
  </el-upload>
</template>

<script setup>
import { ref, defineEmits, defineProps } from 'vue'

import { getToken } from '@/utils/storage'
import { getFileSuffix, downloadFileByResponse } from '@/utils/file'
import { fileClassifyEnum } from '@enum-ms/file'

import { ElUpload, ElMessage } from 'element-plus'

const emit = defineEmits(['success'])

// TODO: 后端暂不支持多文件上传
const props = defineProps({
  data: {
    type: Object,
    default: () => {}
  },
  fileClassify: {
    type: Number,
    default: fileClassifyEnum.NORMAL.V
  },
  action: {
    type: String,
    default: ''
  },
  icon: {
    type: String,
    default: ''
  },
  limit: {
    type: Number,
    default: 1
  },
  multiple: {
    type: Boolean,
    default: false
  },
  accept: {
    type: String,
    default: '.xls,.xlsx'
  },
  uploadFun: {
    type: Function,
    default: undefined
  },
  btnName: {
    type: String,
    default: '点击上传'
  },
  btnType: {
    type: String,
    default: 'primary'
  },
  btnSize: {
    type: String,
    default: 'small'
  },
  disabled: {
    type: Boolean,
    default: false
  },
  sizeLimit: {
    type: Number,
    default: undefined
  },
  successMsg: {
    type: String,
    default: ''
  }
})

const upload = ref()
const headers = ref({ Authorization: getToken() })
const uploadLoading = ref(false)
const fileList = ref([])

function handleExceed(files, fileList) {
  ElMessage.warning(`当前限制选择 ${props.limit} 个文件`)
}

async function handleRequest(file) {
  try {
    uploadLoading.value = true
    const fileObj = file.file
    const formData = new FormData()
    formData.append('file', fileObj)
    formData.append('fileType', props.fileClassify)
    for (const key in props.data) {
      formData.append(key, props.data[key])
    }
    const res = await props.uploadFun(formData)
    const result = res.headers.result && decodeURIComponent(res.headers.result).split(';')
    if (result) {
      // code 20000上传成功，50000上传失败（返回Excel模板或错误提示）
      const code = result[0].split('=')[1]
      const message = result[1].split('=')[1]
      const status = code === '20000'
      ElMessage({
        title: `上传${status ? '成功' : '失败'}`,
        message: status && props.successMsg || `${fileObj.name} ${message}`,
        type: status ? 'success' : 'error',
        duration: 3500
      })
      if (status) {
        emit('success', true)
      } else {
        await downloadFileByResponse(res)
      }
    } else {
      ElMessage.error(`上传失败`)
    }
  } catch (error) {
    console.log(error)
  } finally {
    uploadLoading.value = false
    if (upload.value & upload.value.uploadRef) {
      upload.value.uploadRef.fileList.length = 0
    }
    // handleClear()
  }
}

function handleClear() {
  // TODO: 清空无效
  // upload.value.clearFiles()
  if (upload.value && upload.value.uploadRef) {
    upload.value.uploadRef.fileList.length = 0
  }
}
function handleBefore(file) {
  if (props.accept) {
    const typeFlag = props.accept.split(',').indexOf(`.${getFileSuffix(file.name).toLowerCase()}`) > -1
    if (!typeFlag) {
      ElMessage.error(`上传文件后缀需为${props.accept}格式`, 2000)
      return false
    }
  }
  const sizeM = file.size / 1024 / 1024
  const isLimit = !props.sizeLimit || (props.sizeLimit && sizeM < props.sizeLimit)
  if (!isLimit) {
    ElMessage.error(`上传文件大小大小不能超过 ${props.sizeLimit}MB!`, 2000)
    return false
  }
  return true
}

function handleSuccess(response) {
  if (props.uploadFun) {
    return false
  }
  handleClear()
  uploadLoading.value = false
  if (response && response.code === 20000) {
    emit('success', true)
    ElMessage.success('上传成功')
  } else {
    ElMessage.error(response && response.message ? response.message : '上传失败')
  }
}

function handleError() {
  if (props.uploadFun) {
    return false
  }
  handleClear()
  uploadLoading.value = false
  ElMessage.error('上传失败')
}

function handleProgress() {
  uploadLoading.value = true
}
</script>
