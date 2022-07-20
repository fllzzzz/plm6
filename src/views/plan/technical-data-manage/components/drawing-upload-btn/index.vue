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
    <common-dialog
      title="压缩包内容"
      v-model="dialogVisible"
      width="50%"
      top="10vh"
      :before-close="handleZipDlgClose"
    >
      <span v-if="errorList && errorList.length > 0" class="red-tip" style="display:inline-block;margin-bottom:10px">含 {{ errorList.length }} 个文件后缀不为“ {{ tip }} ”的文件,请重新打包上传</span>
      <div class="zip-box">
        <ul v-if="errorList && errorList.length > 0" class="zip-list">
          <li v-for="(file,index) in errorList" :key="index">
            <span style="color:red">{{ file.name }}</span>
          </li>
        </ul>
        <ul class="zip-list">
          <li v-for="(file,index) in zipList" :key="index">
            <span style="color:green">{{ file.name }}</span>
          </li>
        </ul>
      </div>
      <template #footer>
        <span class="dialog-footer">
          <common-button @click="handleZipDlgClose">取 消</common-button>
          <template v-if="dataType===technicalDataTypeEnum.DEEPEN.V || dataType===technicalDataTypeEnum.MACHINE_PART.V">
            <common-button :loading="uploadZipLoading" type="primary" :disabled="errorList && errorList.length > 0" @click="uploadZip">上传</common-button>
          </template>
          <template v-else>
            <el-popconfirm :title="dataType?'保存后无法删除,确定上传?':'确定上传?'" @confirm="uploadZip">
              <template #reference>
                <common-button :loading="uploadZipLoading" type="primary" :disabled="errorList && errorList.length > 0">上传</common-button>
              </template>
            </el-popconfirm>
          </template>
        </span>
      </template>
    </common-dialog>
  </div>
</template>

<script setup>
import { ref, defineEmits, defineProps } from 'vue'
import { getToken } from '@/utils/storage'
import { getFileSuffix } from '@/utils/file'
import { fileClassifyEnum } from '@enum-ms/file'
import { ElUpload, ElMessage, ElNotification } from 'element-plus'
import { technicalDataTypeEnum } from '@enum-ms/plan'
import JSZip from 'jszip'

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
  fileClassify: {
    type: Number,
    default: fileClassifyEnum.NORMAL.V
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
  // 技术资料类型
  dataType: {
    type: Number,
    default: undefined
  },
  // 材料类型
  materialType: {
    type: Number,
    default: undefined
  },
  tip: {
    type: String,
    default: undefined
  }
})

const headers = ref({ Authorization: getToken() })
const uploadLoading = ref(false)
const fileList = ref([])
const zipList = ref([])
const errorList = ref([])
const uploadZipLoading = ref(false)
const dialogVisible = ref(false)
// const tip = ref()
const currentFile = ref()
const uploadRef = ref()

// watch(
//   () => props.materialType,
//   (val) => {
//     if (val === 2) {
//       tip.value = '.png, .jpg, .jpeg ,.pdf'
//       // 修改可解析的zip格式
//     } else {
//       tip.value = '.pdf'
//     }
//   },
//   { deep: true, immediate: true }
// )

// function handleClose() {
//   dialogVisible.value = false
// }
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
    // formData.append('fileType', props.fileClassify)
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
  console.log(file)
  if (suffix === '.zip') { // 文件类型为zip时做处理
    currentFile.value = file
    handleZip(file)
    return false
  }
}
async function handleZip(zip) {
  const jsZip = new JSZip()
  const zipListData = []
  const errorListData = []
  const tipArr = props.tip && props.tip.length > 0 ? props.tip.split(',') : []
  // 获取解析zip对象
  const resolveZip = await jsZip.loadAsync(zip)
  resolveZip.forEach((relativePath, zipEntry) => { // 2) print entries
    const suffix = `.${getFileSuffix(zipEntry.name)}`.toLowerCase()
    if (zipEntry.name.indexOf('.') > -1) { // 判断是否是目录
      const data = {
        name: zipEntry.name,
        '.PDF': suffix === '.pdf',
        '.JPG': suffix === '.jpg',
        '.PNG': suffix === '.png',
        '.JPEG': suffix === '.jpeg',
        '.DWG': suffix === '.dwg',
        '.DXF': suffix === '.dxf'
      }
      if (props.tip && props.tip.length > 0) {
        const errArr = []
        tipArr.forEach(v => {
          errArr.push(data[v.toUpperCase()])
        })
        if (errArr.indexOf(true) < 0) {
          errorListData.push(data)
        } else {
          zipListData.push(data)
        }
      }
    }
  })
  zipList.value = zipListData
  errorList.value = errorListData
  previewZip() // 预览zip
}
// 上传zip
async function uploadZip() {
  uploadZipLoading.value = true
  try {
    const file = {
      file: currentFile.value
    }
    await handleRequest(file)
  } catch (error) {
    console.log(error)
  } finally {
    uploadZipLoading.value = false
    handleZipDlgClose()
  }
}
function handleExceed(files, fileList) {
  ElMessage({ message: `当前限制选择 ${this.limit} 个文件，本次选择了 ${files.length} 个文件，共选择了 ${files.length + fileList.length} 个文件`, type: 'warning' })
}
function previewZip() {
  dialogVisible.value = true
}
function handleZipDlgClose() {
  dialogVisible.value = false
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
