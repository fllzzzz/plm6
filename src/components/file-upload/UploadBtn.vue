<template>
  <el-upload
    ref="upload"
    class="upload-box"
    :data="{ ...props.data, fileType: props.fileClassify }"
    :action="fileUploadApi"
    :headers="headers"
    :on-success="handleSuccess"
    :on-error="handleError"
    :on-progress="handleProgress"
    :on-remove="handleRemove"
    :before-upload="beforeUpload"
    :before-remove="beforeRemove"
    :limit="props.limit"
    :show-file-list="props.showFileList"
    :on-exceed="handleExceed"
    :disabled="props.disabled || uploadLoading"
    :accept="props.accept"
    multiple
  >
    <common-button :loading="uploadLoading" :size="props.size" :icon="props.icon" :disabled="props.disabled || uploadLoading" :type="props.btnType">{{ props.btnName }}</common-button>
    <template v-slot:tip>
      <div v-if="props.tip" class="el-upload__tip">{{ props.tip }}</div>
    </template>
  </el-upload>
</template>

<script setup>
import { ref, defineEmits, defineProps, watch, defineExpose } from 'vue'
import { mapGetters } from '@/store/lib'

import { getToken } from '@/utils/storage'
import { fileClassifyEnum } from '@enum-ms/file'

import { ElUpload, ElMessage, ElMessageBox } from 'element-plus'

const emit = defineEmits(['update:files', 'change'])

// TODO: 后端暂不支持多文件上传
const props = defineProps({
  data: {
    type: Object,
    default: () => {}
  },
  files: {
    type: Array,
    default: () => []
  },
  fileClassify: {
    type: Number,
    default: fileClassifyEnum.NORMAL.V
  },
  limit: {
    type: Number,
    default: undefined
  },
  hasBeforeConfirm: {
    type: Boolean,
    default: false
  },
  beforeMessage: {
    type: String,
    default: '确认上传吗？'
  },
  successMessage: {
    type: String,
    default: '上传成功'
  },
  showFileList: {
    type: Boolean,
    default: true
  },
  size: {
    type: String,
    default: 'mini'
  },
  accept: {
    type: String,
    default: ''
  },
  icon: {
    type: String,
    default: ''
  },
  btnName: {
    type: String,
    default: '点击上传'
  },
  btnType: {
    type: String,
    default: 'primary'
  },
  tip: {
    type: String,
    default: ''
  },
  disabled: {
    type: Boolean,
    default: false
  }
})

const { fileUploadApi } = mapGetters('fileUploadApi')

const upload = ref()
const headers = ref({ Authorization: getToken() })
const uploadLoading = ref(false)

watch(
  () => props.files,
  (val) => {
    if (!val || val.length === 0) {
      clearFiles()
    }
  },
  { deep: true }
)

function handleSuccess(response, file, fileList) {
  uploadLoading.value = false
  if (response && response.code === 20000) {
    const f = response.data
    const files = [...(props.files || [])]
    files.push(Object.assign(f, { uid: file.uid }))
    emit('update:files', files)
    emit('change')
    ElMessage.success(props.successMessage)
  } else {
    ElMessage.error(response && response.message ? response.message : '上传失败')
  }
}

function handleError() {
  uploadLoading.value = false
  ElMessage.error('上传失败')
}

function handleRemove(file, fileList) {
  for (const i in props.files) {
    if (file.uid === props.files[i].uid) {
      const files = [...props.files]
      files.splice(i, 1)
      emit('update:files', files)
      emit('change')
      break
    }
  }
}

function handleExceed(files, fileList) {
  ElMessage.warning(`当前限制选择 ${props.limit} 个文件`)
}
function beforeUpload() {
  if (props.hasBeforeConfirm) {
    return ElMessageBox.confirm(props.beforeMessage)
  } else {
    return true
  }
}
function beforeRemove(file, fileList) {
  if (file && file.status === 'success') {
    return ElMessageBox.confirm(`确定移除 ${file.name}？`)
  }
}

function clearFiles() {
  if (upload.value && upload.value.uploadRef) {
    upload.value.uploadRef.fileList.length = 0
  }
}

function handleProgress() {
  uploadLoading.value = true
}

defineExpose({
  clearFiles
})
</script>
