/* eslint-disable vue/no-mutating-props */
<template>
  <el-upload
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
    <el-button :size="size" :icon="icon" :disabled="disabled" :type="btnType">{{ btnName }}</el-button>
    <template v-slot:tip>
      <div v-if="tip" class="el-upload__tip">{{ tip }}</div>
    </template>
  </el-upload>
</template>

<script>
import { mapGetters } from 'vuex'
import { getFileSuffix } from '@/utils'
import { getToken } from '@/utils/auth'
import { fileClassifyEnum } from '@/utils/enum/index'

// TODO: 后端暂不支持多文件上传
export default {
  props: {
    data: {
      type: Object
    },
    files: {
      type: Array,
      default: () => []
    },
    fileClassify: {
      type: Number,
      default: fileClassifyEnum.OTHER.V
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
  },
  data() {
    return {
      headers: {
        Authorization: `cat ${getToken()}`
      },
      downloadLoading: false,
      uploadLoading: false
      // fileList: []
    }
  },
  computed: {
    ...mapGetters(['fileUploadApi'])
  },
  watch: {
    files: {
      handler(val) {
        if (!val || val.length === 0) {
          this.clearFiles()
        }
      },
      deep: true
    }
  },
  methods: {
    handleSuccess(response, file, fileList) {
      if (response && response.code === 20000) {
        const f = response.data
        // eslint-disable-next-line vue/no-mutating-props
        this.files.push(Object.assign(f, { uid: file.uid }))
        this.$emit('update:files', this.files)
        this.$emit('change')
        this.$notify({ title: this.successMessage, type: 'success', duration: 2500 })
      } else {
        this.$notify({ title: response && response.message ? response.message : '上传失败', type: 'error', duration: 2500 })
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
    handleError() {
      this.$message({ message: '上传失败', type: 'error' })
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
    }
  }
}
</script>
