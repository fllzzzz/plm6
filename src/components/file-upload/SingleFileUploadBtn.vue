<template>
  <el-upload
    ref="upload"
    :data="{ ...data, fileType: fileClassify }"
    :action="action"
    :headers="headers"
    :http-request="handleRequest"
    :before-upload="handleBefore"
    :on-success="handleSuccess"
    :on-progress="handleProgress"
    :on-error="handleError"
    :limit="limit"
    :multiple="multiple"
    :on-exceed="handleExceed"
    :file-list="fileList"
    :show-file-list="false"
    :accept="accept"
    :disabled="disabled"
  >
    <el-button :loading="uploadLoading" :disabled="disabled" :icon="icon" :size="btnSize" :type="btnType">{{ btnName }}</el-button>
  </el-upload>
</template>

<script>
import { mapGetters } from 'vuex'
import { getFileSuffix } from '@/utils'
import { getToken } from '@/utils/auth'
import { fileClassifyEnum } from '@/utils/enum/index'

export default {
  name: 'UploadSingleFileBtn',
  props: {
    // eslint-disable-next-line vue/require-default-prop
    data: {
      type: Object
    },
    fileClassify: {
      type: Number,
      default: fileClassifyEnum.OTHER.V
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
    // eslint-disable-next-line vue/require-default-prop
    uploadFun: {
      type: Function
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
  },
  data() {
    return {
      headers: {
        Authorization: `cat ${getToken()}`
      },
      uploadLoading: false,
      fileList: []
    }
  },
  computed: {
    ...mapGetters(['token'])
  },
  methods: {
    handleExceed(files, fileList) {
      this.$message.warning(`当前限制选择 ${this.limit} 个文件，本次选择了 ${files.length} 个文件，共选择了 ${files.length + fileList.length} 个文件`)
    },
    async handleRequest(file) {
      try {
        this.uploadLoading = true
        const fileObj = file.file
        const formData = new FormData()
        formData.append('file', fileObj)
        formData.append('fileType', this.fileClassify)
        for (const key in this.data) {
          formData.append(key, this.data[key])
        }
        await this.uploadFun(formData)
        this.$notify({
          title: '上传成功',
          type: 'success',
          message: this.successMsg || `${fileObj.name} 上传成功`,
          duration: 2500
        })
        this.$emit('success', true)
      } catch (error) {
        console.log(error)
      } finally {
        this.uploadLoading = false
        this.$refs.upload.clearFiles()
      }
    },
    handleBefore(file) {
      if (this.accept) {
        const typeFlag = this.accept.split(',').indexOf(`.${getFileSuffix(file.name).toLowerCase()}`) > -1
        if (!typeFlag) {
          this.$message.error(`上传文件后缀需为${this.accept}格式`, 2000)
          return false
        }
      }
      const sizeM = file.size / 1024 / 1024
      const isLimit = !this.sizeLimit || (this.sizeLimit && sizeM < this.sizeLimit)
      if (!isLimit) {
        this.$message.error(`上传文件大小大小不能超过 ${this.sizeLimit}MB!`, 2000)
        return false
      }
      return true
    },
    handleSuccess(response) {
      if (this.uploadFun) {
        return false
      }
      this.$refs.upload.clearFiles()
      this.uploadLoading = false
      if (response && response.code === 20000) {
        this.$notify({
          title: '上传成功',
          type: 'success',
          message: this.successMsg || undefined,
          duration: 2500
        })
        this.$emit('success', true)
      } else {
        this.$notify({
          title: response && response.message ? response.message : '上传失败',
          type: 'error',
          duration: 2500
        })
      }
    },
    handleError() {
      if (this.uploadFun) {
        return false
      }
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
