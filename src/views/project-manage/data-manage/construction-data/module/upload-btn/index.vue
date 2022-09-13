<template>
  <div>
    <el-upload
      ref="uploadRef"
      action=""
      :headers="headers"
      :http-request="handleRequest"
      :before-upload="handleBefore"
      :limit="props.limit"
      :multiple="false"
      :on-exceed="handleExceed"
      :file-list="fileList"
      :show-file-list="false"
      :accept="props.accept"
    >
      <common-button :loading="uploadLoading" :icon="props.icon" :size="props.btnSize" :type="props.btnType" :disabled="props.disabled">{{ btnName }}</common-button>
    </el-upload>
  </div>
</template>

<script setup>
import { ref, defineEmits, defineProps } from 'vue'
import { getToken } from '@/utils/storage'
import { getFileSuffix } from '@/utils/file'
import { ElUpload, ElMessage, ElNotification } from 'element-plus'

const emit = defineEmits(['success'])
const props = defineProps({
  data: {
    type: Object,
    default: () => {}
  },
  uploadFun: {
    type: Function,
    default: undefined
  },
  icon: {
    type: String,
    default: ''
  },
  limit: {
    type: Number,
    default: 1
  },
  accept: {
    type: String,
    default: undefined
  },
  btnType: {
    type: String,
    default: 'primary'
  },
  btnSize: {
    type: String,
    default: 'small'
  },
  btnName: {
    type: String,
    default: '文件上传'
  },
  sizeLimit: {
    type: Number,
    default: undefined
  },
  disabled: {
    type: Boolean,
    default: false
  },
  successMsg: {
    type: String,
    default: ''
  },
  tip: {
    type: String,
    default: undefined
  }
})

const headers = ref({ Authorization: getToken() })
const uploadLoading = ref(false)
const fileList = ref([])
const uploadRef = ref()

async function handleRequest(file) {
  try {
    uploadLoading.value = true
    const fileObj = file.file
    const formData = new FormData() // 添加参数
    if (props.data.id) {
      formData.append('multipartFile', fileObj)
    } else {
      formData.append('file', fileObj)
    }
    for (const key in props.data) {
      if (props.data[key]) {
        formData.append(key, props.data[key])
      }
    }
    await props.uploadFun(formData)
    ElNotification({
      title: '上传成功',
      type: 'success',
      message: props.successMsg || `${fileObj.name} 上传成功`,
      duration: 2500
    })
    emit('success', true)
  } catch (error) {
    console.log(error)
  } finally {
    uploadLoading.value = false
    handleClear() // 清空文件
  }
}

function handleClear() {
  // TODO: 清空无效
  // uploadRef.value.clearFiles()
  if (uploadRef.value && uploadRef.value.uploadRef) {
    uploadRef.value.uploadRef.fileList.length = 0
  }
}

function handleBefore(file) {
  const suffix = `.${getFileSuffix(file.name)}`.toLowerCase()
  const sizeM = file.size / 1024 / 1024
  const isLimit = !props.sizeLimit || (props.sizeLimit && sizeM < props.sizeLimit)
  if (props.accept) {
    const typeFlag = props.accept.split(',').indexOf(suffix) > -1
    // 判断文件格式
    if (!typeFlag) {
      ElMessage({ message: `上传文件后缀需为${props.accept}格式`, type: 'error' })
      return false
    }
  }
  // 判断文件大小
  if (!isLimit) {
    ElMessage({ message: `上传文件大小不能超过 ${props.sizeLimit}MB!`, type: 'error' })
    return false
  }
}

function handleExceed(files, fileList) {
  ElMessage({ message: `当前限制选择 ${this.limit} 个文件，本次选择了 ${files.length} 个文件，共选择了 ${files.length + fileList.length} 个文件`, type: 'warning' })
}

</script>

<style lang="scss" scoped>
.zip-box {
  overflow: auto;
  height: 60vh;
}
::v-deep(.el-dialog__body){
    padding: 10px 20px;
    .zip-list {
      padding: 0 10px;
      &>li{
        white-space: nowrap;
        line-height: 25px;
      }
    }
}
</style>
