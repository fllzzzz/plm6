<template>
  <div class="upload-container">
    <el-upload
      ref="upload"
      class="upload-box"
      drag
      v-if="!props.isShow"
      :on-success="handleSuccess"
      :on-error="handleError"
      :on-progress="handleProgress"
      :headers="headers"
      :action="fileUploadApi"
      :data="{ fileType: props.fileClassify }"
      multiple
    >
      <i class="el-icon-upload" />
      <div class="el-upload__text">将文件拖到此处，或<em>点击上传</em></div>
      <template v-slot:tip>
        <div class="el-upload__tip">请填写正确的文件名，文件名上传后不可更改</div>
      </template>
    </el-upload>
    <div class="attachment-content" style="width: 100%">
      <common-table :data="files">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="name" label="名称" :show-overflow-tooltip="true" min-width="200" />
        <el-table-column prop="createTime" label="上传时间" :show-overflow-tooltip="true" min-width="180">
          <template v-slot="scope">
            <span v-parse-time="'{y}-{m}-{d}'">{{scope.row.createTime}}</span>
          </template>
        </el-table-column>
        <el-table-column label="操作" width="130px">
          <template v-slot="scope">
            <common-button v-if="!isShow" type="danger" icon="el-icon-delete" size="mini" @click="toDelete(scope.$index)" />
            <common-button v-show="props.showDownload && currentUpload.indexOf(scope.row.id) == -1" v-permission="props.downloadPerm" :loading="downloadLoading" size="mini" type="warning" icon="el-icon-download" @click="downloadFile(scope.$index)" />
          </template>
        </el-table-column>
      </common-table>
    </div>
  </div>
</template>

<script setup>
import { fileDownload } from '@/utils/file'
import { ref, defineEmits, defineProps } from 'vue'
import { mapGetters } from '@/store/lib'

import { getToken } from '@/utils/storage'
import { fileClassifyEnum } from '@enum-ms/common'

import { ElUpload, ElMessage } from 'element-plus'

const emit = defineEmits(['update:files'])

// TODO: 后端暂不支持多文件上传
const props = defineProps({
  files: {
    type: Array,
    default: () => []
  },
  fileClassify: {
    type: Number,
    default: fileClassifyEnum.OTHER.V
  },
  downloadFn: {
    type: Function,
    default: undefined
  },
  downloadPerm: {
    type: Array,
    default: () => []
  },
  downloadParams: {
    type: Object,
    default: undefined
  },
  showDownload: {
    type: Boolean,
    default: false
  },
  // 是否是查看状态
  isShow: {
    type: Boolean,
    default: false
  }
})

const { fileUploadApi } = mapGetters('fileUploadApi')

const upload = ref()
const headers = ref({ Authorization: getToken() })
const uploadLoading = ref(false)
const downloadLoading = ref(false)
const currentUpload = ref([])

function toDelete(index) {
  const files = [...props.files]
  files.splice(index, 1)
  emit('update:files', files)
}

async function downloadFile(index) {
  downloadLoading.value = true
  try {
    const params = { ...props.downloadParams }
    params.id = props.files[index].id
    await fileDownload(props.downloadFn, params)
  } catch (error) {
    console.log('下载附件', error)
  } finally {
    downloadLoading.value = false
  }
}

function handleSuccess(response) {
  upload.value.clearFiles()
  uploadLoading.value = false
  if (response && response.code === 20000) {
    const data = response.data
    emit('update:files', props.files.concat(response.data))
    currentUpload.value = currentUpload.value.concat(data.map(v => v.id))
    ElMessage.success('上传成功')
  } else {
    ElMessage.error(response && response.message ? response.message : '上传失败')
  }
}

function handleError() {
  upload.value.clearFiles()
  uploadLoading.value = false
  ElMessage.error('上传失败')
}

function handleProgress() {
  uploadLoading.value = true
}
</script>

<style lang="scss" scoped>
.upload-container {
  width: 100%;
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: flex-start;

  .upload-box {
    width: 400px;
    margin: 10px 20px 0 0;
  }
}
</style>
