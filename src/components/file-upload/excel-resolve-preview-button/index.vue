<template>
  <div>
    <el-upload
      ref="uploadRef"
      class="upload-excel-resolve"
      action=""
      :before-upload="handleBefore"
      :on-change="handleChange"
      :on-exceed="handleExceed"
      :disabled="props.disabled"
      :limit="props.limit"
      :accept="props.accept"
      :auto-upload="false"
      :show-file-list="false"
    >
      <common-button
        v-if="props.disabled"
        :loading="props.resolveLoading"
        :disabled="props.disabled"
        :icon="props.icon"
        :size="props.btnSize"
        :type="props.btnType"
        >{{ props.btnName }}</common-button
      >
      <el-button
        v-else
        :loading="props.resolveLoading"
        :disabled="props.disabled"
        :icon="props.icon"
        :size="props.btnSize"
        :type="props.btnType"
        >{{ props.btnName }}</el-button
      >
    </el-upload>
    <common-dialog
      :title="props.title"
      v-model="previewVisible"
      width="800px"
      :before-close="handleClose"
      :show-close="true"
      :close-on-click-modal="false"
      custom-class="excel-resolve-preview"
      top="10vh"
    >
      <template #titleRight>
        <common-button :loading="submitLoading" type="primary" size="mini" @click="submit">提 交</common-button>
      </template>
      <common-table ref="table" :data="list" empty-text="暂无数据" :max-height="maxHeight" default-expand-all style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <template v-for="item in props.template.fields" :key="item.field">
          {{item.name}}
          <el-table-column :prop="item.field" :show-overflow-tooltip="true" :label="item.label" />
        </template>
      </common-table>
    </common-dialog>
  </div>
</template>
<script setup>
import { defineEmits, defineProps, computed, ref } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useTableValidate from '@compos/form/use-table-validate'
import { ElUpload, ElMessage, ElButton } from 'element-plus'
import { resolveExcel, fileVerification, formatExcelData } from '@/utils/file'

const emit = defineEmits(['data'])

const props = defineProps({
  title: {
    type: String,
    default: '表格解析预览'
  },
  template: {
    type: Object,
    required: true
  },
  accept: {
    type: String,
    default: '.xls,.xlsx'
  },
  successMessage: {
    type: String,
    default: '上传成功'
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
    default: undefined
  },
  sizeLimit: {
    type: Number,
    default: undefined
  }
})

const previewVisible = ref(false)
// 提交loading
const submitLoading = ref(false)
const list = computed(() => {
  return formatExcelData(resolveData.value, props.template)
})

// const { tableValidate, wrongCellMask } = useTableValidate({ rules: currentRules })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.cls-batch-add',
    extraBox: ['.el-dialog__header', '.heade-operate'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false
  },
  previewVisible
)

// 提交表单
async function submit() {
  submitLoading.value = true
  try {
    emit('success')
    ElMessage.success('添加成功')
    handleClose()
  } catch (error) {
    console.log('科目添加', error)
  } finally {
    submitLoading.value = false
  }
}

const handleClose = () => {
  previewVisible.value = false
}

// ------------------------------- 处理文件 -------------------------------
const uploadRef = ref()
const resolveLoading = ref(false)
const resolveData = ref([])

// 处理前
function handleBefore(file) {
  previewVisible.value = true
  fileVerification(file, {
    accept: props.accept,
    sizeLimit: props.sizeLimit
  })
  return true
}

async function handleChange(file, fileList) {
  try {
    previewVisible.value = true
    resolveLoading.value = true
    // TODO: 清空无效
    // uploadRef.value.clearFiles()
    resolveData.value = await resolveExcel(file.raw)
  } catch (error) {
    console.log('excel文件解析', error)
  } finally {
    resolveLoading.value = false
  }
}

function handleExceed(files, fileList) {
  ElMessage.warning(
    `当前限制选择 ${props.limit} 个文件，本次选择了 ${files.length} 个文件，共选择了 ${files.length + fileList.length} 个文件`
  )
}
</script>
