<template>
  <div class="upload-container">
    <div class="attachment-content">
      <common-table :data="curFiles" :empty-text="emptyText" style="width: 100%" :max-height="props.maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="name" label="名称" :show-overflow-tooltip="true" min-width="150" />
        <el-table-column prop="createTime" label="上传时间" :show-overflow-tooltip="true" width="100" align="center">
          <template #default="{ row }">
            <span v-parse-time="{ val: row.createTime, fmt: '{y}-{m}-{d}' }" />
          </template>
        </el-table-column>
        <el-table-column label="操作" :width="uploadable && props.showDownload ? 117 : 67" align="left">
          <template #default="{ row, $index }">
            <common-button v-if="uploadable" type="danger" icon="el-icon-delete" size="mini" @click="toDelete($index)" />
            <export-button
              v-show="props.showDownload"
              v-permission="props.downloadPerm"
              :params="getParams(row, $index)"
              :fn="props.downloadFn"
            />
          </template>
        </el-table-column>
      </common-table>
      <el-upload
        v-if="uploadable"
        ref="uploadRef"
        class="upload-box"
        :data="{ ...props.data, fileType: props.fileClassify }"
        :action="fileUploadApi"
        :headers="headers"
        :on-success="handleSuccess"
        :on-error="handleError"
        :on-remove="handleRemove"
        :on-progress="handleProgress"
        :before-upload="beforeUpload"
        :before-remove="beforeRemove"
        :limit="props.limit"
        :show-file-list="props.showFileList"
        :on-exceed="handleExceed"
        :disabled="props.disabled"
        multiple
      >
        <common-button :loading="uploadLoading" :size="props.size" :icon="props.icon" :disabled="props.disabled" :type="props.btnType">
          <span v-if="props.btnName">{{ props.btnName }}</span>
        </common-button>
      </el-upload>
    </div>
  </div>
</template>

<script setup>
import { ref, defineEmits, defineProps, watchEffect } from 'vue'
import { mapGetters } from '@/store/lib'

import { getToken } from '@/utils/storage'
import { fileClassifyEnum } from '@enum-ms/file'

import { ElUpload, ElMessage, ElMessageBox } from 'element-plus'
import ExportButton from '@comp-common/export-button/index.vue'

const emit = defineEmits(['update:files', 'change'])

// TODO: 后端暂不支持多文件上传
// 将fileUploadApi 更改为调用方法上传
const props = defineProps({
  data: {
    type: Object,
    default: undefined
  },
  files: {
    type: Array,
    default: () => []
  },
  fileClassify: {
    type: Number,
    default: fileClassifyEnum.NORMAL.V
  },
  downloadFn: {
    type: Function,
    default: undefined
  },
  downloadPerm: {
    type: Array,
    default: undefined
  },
  downloadParams: {
    type: Object,
    default: () => {
      return {}
    }
  },
  showDownload: {
    type: Boolean,
    default: false
  },
  uploadable: {
    type: Boolean,
    default: true
  },
  limit: {
    type: Number,
    default: undefined
  },
  emptyText: {
    type: String,
    default: '暂未上传文件'
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
    default: false
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
  disabled: {
    type: Boolean,
    default: false
  },
  maxHeight: {
    type: [String, Number]
  }
})

const { fileUploadApi } = mapGetters('fileUploadApi')

const uploadRef = ref()
const headers = ref({ Authorization: getToken() })
const uploadLoading = ref(false)
const currentUpload = ref([])
const curFiles = ref()

watchEffect(() => {
  curFiles.value = props.files || []
})

function getParams(row, index) {
  const id = index >= 0 && index <= curFiles.value.length - 1 ? curFiles.value[index].id : undefined
  return { ...props.downloadParams, id }
}

function toDelete(index) {
  const files = [...curFiles.value]
  files.splice(index, 1)
  emit('update:files', files)
}

function handleSuccess(response) {
  handleClear()
  uploadLoading.value = false
  if (response && response.code === 20000) {
    const data = [response.data]
    emit('update:files', curFiles.value.concat(response.data))
    currentUpload.value = currentUpload.value.concat(data.map((v) => v.id))
    ElMessage.success('上传成功')
  } else {
    ElMessage.error(response && response.message ? response.message : '上传失败')
  }
}

function handleClear() {
  // TODO: 清空无效
  // uploadRef.value.clearFiles()
  if (uploadRef.value && uploadRef.value.uploadRef) {
    uploadRef.value.uploadRef.fileList.length = 0
  }
}

function handleError() {
  handleClear()
  uploadLoading.value = false
  ElMessage.error('上传失败')
}

function handleProgress() {
  uploadLoading.value = true
}

function handleRemove(file, fileList) {
  for (const i in curFiles.value) {
    if (file.uid === curFiles.value[i].uid) {
      const files = [...curFiles.value]
      files.splice(i, 1)
      emit('update:files', files)
      emit('change')
      break
    }
  }
}
function handleExceed(files, fileList) {
  ElMessage.warning(
    `当前限制选择 ${props.limit} 个文件`
  )
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
</script>

<style lang="scss" scoped>
.upload-container {
  width: 100%;
  .upload-box {
    position: absolute;
    display: inline-block;
    right: 13px;
    top: 6px;
  }
  .attachment-content {
    position: relative;
  }
}
</style>
