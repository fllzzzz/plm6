<template>
  <div class="upload-container">
    <el-upload
v-if="!isShow"
ref="upload"
class="upload-box"
drag
:on-success="handleSuccess"
:on-error="handleError"
:on-progress="handleProgress"
:headers="headers"
:action="fileUploadApi"
:data="{ fileType: fileClassify }"
multiple>
      <i class="el-icon-upload" />
      <div class="el-upload__text">将文件拖到此处，或<em>点击上传</em></div>
      <template v-slot:tip>
        <div class="el-upload__tip">请填写正确的文件名，文件名上传后不可更改</div>
      </template>
    </el-upload>
    <div class="attachment-content">
      <el-table :data="files" style="width: 100%">
        <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号' : ''" type="index" align="center" width="60" />
        <el-table-column prop="name" label="名称" :show-overflow-tooltip="true" min-width="200" />
        <el-table-column prop="createTime" label="上传时间" :show-overflow-tooltip="true" min-width="180">
          <template v-slot="scope">
            {{ parseTime(scope.row.createTime) }}
          </template>
        </el-table-column>
        <el-table-column label="操作" width="130px">
          <template v-slot="scope">
            <el-button v-if="!isShow" type="danger" icon="el-icon-delete" size="mini" @click="toDelete(scope.$index)" />
            <el-button v-show="showDownload && currentUpload.indexOf(scope.row.id) == -1" v-permission="downloadPerm" :loading="downloadLoading" size="mini" type="warning" icon="el-icon-download" @click="downloadFile(scope.$index)" />
          </template>
        </el-table-column>
      </el-table>
    </div>
  </div>
</template>

<script>
import { mapGetters } from 'vuex'
import { getToken } from '@/utils/auth'
import { fileDownload } from '@/utils/other'
import { fileClassifyEnum } from '@/utils/enum/index'
import { parseTime } from '@/filters'

// TODO: 后端暂不支持多文件上传
export default {
  props: {
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
        console.log('合同附件', error)
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
    handleProgress() {
      this.uploadLoading = true
    }
  }
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
  .attachment-content {
    max-width: 700px;
  }
}
</style>
