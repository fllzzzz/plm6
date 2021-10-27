<template>
  <div class="upload-container">
    <div class="attachment-content">
      <common-table :data="files" :empty-text="emptyText" style="width: 100%">
        <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号' : ''" type="index" align="center" width="60" />
        <el-table-column prop="name" label="名称" :show-overflow-tooltip="true" min-width="200" />
        <el-table-column prop="createTime" label="上传时间" :show-overflow-tooltip="true" min-width="180">
          <template v-slot="scope">
            {{ parseTime(scope.row.createTime) }}
          </template>
        </el-table-column>
        <el-table-column label="操作" width="200px">
          <template v-slot="scope">
            <el-button v-if="uploadable" type="danger" icon="el-icon-delete" size="mini" @click="toDelete(scope.$index)" />
            <export-button
              v-show="showDownload && currentUpload.indexOf(scope.row.id) == -1 && downloadFn && (!downloadPerm || checkPermission(downloadPerm))"
              v-permission="downloadPerm"
              :params="{ ...downloadParams, id: files[scope.$index].id }"
              :fn="downloadFn"
              size="mini"
              type="warning"
              icon="el-icon-download"
            />
            <!-- <el-button v-show="showDownload && currentUpload.indexOf(scope.row.id) == -1 && downloadFn &&(!downloadPerm || checkPermission(downloadPerm))" :loading="downloadLoading" size="mini" type="warning" icon="el-icon-download" @click="downloadFile(scope.$index)" /> -->
          </template>
        </el-table-column>
      </common-table>
      <el-upload
        v-if="uploadable"
        ref="upload"
        class="upload-box"
        :data="{ ...data, fileType: fileClassify }"
        :action="fileUploadApi"
        :headers="headers"
        :on-success="handleSuccess"
        :on-error="handleError"
        :on-preview="handlePreview"
        :on-remove="handleRemove"
        :on-change="handleChange"
        :before-upload="beforeUpload"
        :before-remove="beforeRemove"
        :limit="limit"
        :show-file-list="showFileList"
        :on-exceed="handleExceed"
        :disabled="disabled"
        multiple
      >
        <el-button
          :loading="uploadLoading"
          :size="size"
          :icon="icon"
          :disabled="disabled"
          :type="btnType"
          >
          <span v-if="btnName">{{ btnName }}</span>
        </el-button>
      </el-upload>
    </div>
  </div>
</template>

<script>
import { mapGetters } from 'vuex'
import { getToken } from '@/utils/auth'
import { fileDownload } from '@/utils/other'
import { fileClassifyEnum } from '@/utils/enum/index'
import checkPermission from '@/utils/permission'
import { parseTime } from '@/filters'

// TODO: 后端暂不支持多文件上传
export default {
  props: {
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
      default: fileClassifyEnum.OTHER.V
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
      default: undefined
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
    }
  },
  data() {
    return {
      headers: {
        Authorization: `cat ${getToken()}`
      },
      uploadLoading: false,
      downloadLoading: false,
      currentUpload: []
    }
  },
  computed: {
    ...mapGetters(['fileUploadApi']),
    parseTime
  },
  methods: {
    checkPermission,
    toDelete(index) {
      const files = [...this.files]
      files.splice(index, 1)
      this.$emit('update:files', files)
    },
    async downloadFile(index) {
      this.downloadLoading = true
      try {
        const params = { ...this.downloadParams }
        params.id = this.files[index].id
        // await this.downloadFn(params)
        await fileDownload(this.downloadFn, params)
      } catch (error) {
        console.log('下载附件', error)
      } finally {
        this.downloadLoading = false
      }
    },
    handleSuccess(response) {
      this.$refs.upload.clearFiles()
      this.uploadLoading = false
      if (response && response.code === 20000) {
        const data = response.data
        this.$emit('update:files', this.files.concat(response.data))
        console.log(data)
        this.currentUpload = this.currentUpload.concat(data.map(v => v.id))
        this.$notify({
          title: '上传成功',
          type: 'success',
          duration: 2500
        })
      } else {
        this.$notify({
          title: response && response.message ? response.message : '上传失败',
          type: 'error',
          duration: 2500
        })
      }
    },
    handleError() {
      this.$refs.upload.clearFiles()
      this.uploadLoading = false
      this.$message({
        message: '上传失败',
        type: 'error'
      })
    },
    handleRemove(file, fileList) {
      for (const i in this.files) {
        if (file.uid === this.files[i].uid) {
          // eslint-disable-next-line vue/no-mutating-props
          this.files.splice(i, 1)
          this.$emit('update:files', this.files)
          this.$emit('change')
          break
        }
      }
    },
    handleChange(file, fileList) {},
    handlePreview(file) {},
    handleExceed(files, fileList) {
      this.$message.warning(`当前限制选择 ${this.limit} 个文件，本次选择了 ${files.length} 个文件，共选择了 ${files.length + fileList.length} 个文件`)
    },
    beforeUpload() {
      if (this.hasBeforeConfirm) {
        return this.$confirm(this.beforeMessage)
      } else {
        return true
      }
    },
    beforeRemove(file, fileList) {
      if (file && file.status === 'success') {
        return this.$confirm(`确定移除 ${file.name}？`)
      }
    },
    clearFiles() {
      this.$refs.upload.clearFiles()
    },
    handleProgress() {
      this.uploadLoading = true
    }
  }
}
</script>

<style lang="scss" scoped>
.upload-container {
  width: 100%;
  // display: flex;
  // flex-direction: row;
  // justify-content: center;
  // align-items: flex-start;
  .upload-box {
    position: absolute;
    display: inline-block;
    margin-left: 20px;
    right: 0;
    top: 5px;
  }
  .attachment-content {
    position: relative;
  }
}
</style>
