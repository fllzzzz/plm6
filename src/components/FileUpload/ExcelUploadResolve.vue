<template>
  <el-upload
    ref="upload"
    class="upload-demo"
    action=""
    :before-upload="handleBefore"
    :on-change="handleChange"
    :on-exceed="handleExceed"
    :on-remove="handleRemove"
    :disabled="disabled"
    :limit="limit"
    :multiple="multiple"
    :show-file-list="showFileList"
    :accept="accept"
    :auto-upload="false"
  >
    <el-button :loading="uploadLoading" :disabled="disabled" :icon="icon" :size="btnSize" :type="btnType">{{ btnName }}</el-button>
  </el-upload>
</template>
<script>
import { getFileSuffix } from '@/utils'

export default {
  props: {
    accept: {
      type: String,
      default: '.xls,.xlsx'
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
      default: 1
    },
    sizeLimit: {
      type: Number,
      default: undefined
    },
    multiple: {
      type: Boolean,
      default: false
    }
  },
  data: function() {
    return {
      uploadLoading: false
    }
  },
  methods: {
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
    handleChange(file, fileList) {
      this.resolve(file.raw)
      this.$refs.upload.clearFiles()
    },
    handleExceed(files, fileList) {
      this.$message.warning(`当前限制选择 ${this.limit} 个文件，本次选择了 ${files.length} 个文件，共选择了 ${files.length + fileList.length} 个文件`)
    },
    handleRemove(file, fileList) {
      this.fileTemp = null
    },
    resolve(obj) {
      const _this = this
      // 通过DOM取文件数据
      this.file = obj
      var rABS = false // 是否将文件读取为二进制字符串
      var f = this.file
      var reader = new FileReader()
      // if (!FileReader.prototype.readAsBinaryString) {
      FileReader.prototype.readAsBinaryString = function(f) {
        var binary = ''
        var rABS = false // 是否将文件读取为二进制字符串
        // eslint-disable-next-line no-unused-vars
        var pt = this
        var wb // 读取完成的数据
        var outdata
        var reader = new FileReader()
        reader.onload = (e) => {
          var bytes = new Uint8Array(reader.result)
          var length = bytes.byteLength
          for (var i = 0; i < length; i++) {
            binary += String.fromCharCode(bytes[i])
          }
          var XLSX = require('xlsx-styleable')
          if (rABS) {
            // eslint-disable-next-line no-undef
            wb = XLSX.read(btoa(fixdata(binary)), { // 手动转化
              type: 'base64'
            })
          } else {
            wb = XLSX.read(binary, {
              type: 'binary'
            })
          }
          outdata = XLSX.utils.sheet_to_json(wb.Sheets[wb.SheetNames[0]])// outdata就是你想要的东西
          _this.$emit('success', [...outdata])
          // _this.uploadLoading = false
          // return arr
        }
        reader.readAsArrayBuffer(f)
      }

      if (rABS) {
        reader.readAsArrayBuffer(f)
      } else {
        reader.readAsBinaryString(f)
      }
    },
    clearFiles() {
      this.$refs.upload.clearFiles()
    }
  }
}
</script>
