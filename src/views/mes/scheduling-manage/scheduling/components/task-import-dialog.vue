<template>
  <common-dialog title="任务导入" v-model="dialogVisible" width="500px" :before-close="handleClose">
    <template #titleRight>
      <common-button size="mini" type="primary" @click="saveIt">确认下发</common-button>
    </template>
    <div class="red-tip" style="margin-bottom: 20px">
      <span>* 注意：</span>
      <span>此操作将可将任务直接下发至生产线，请仔细核对表格数据！</span>
    </div>
    <el-form label-width="110px" label-position="left">
      <!-- <el-form-item label="要求完成时间">
        <el-date-picker
          v-model="form.askCompleteTime"
          type="date"
          size="mini"
          style="width: 250px"
          :disabledDate="(v) => moment(v).valueOf() < moment().subtract(1, 'days').valueOf()"
          placeholder="需求完成日期"
        />
      </el-form-item> -->
      <el-form-item label="任务导入">
        <el-upload
          ref="uploadRef"
          action=""
          :accept="'.xls,.xlsx'"
          :http-request="handleRequest"
          :on-exceed="handleExceed"
          :auto-upload="false"
          :limit="1"
        >
          <common-button size="mini" :loading="uploadLoading" type="warning">选择文件</common-button>
        </el-upload>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { taskImport } from '@/api/mes/scheduling-manage/scheduling/common'
import { defineEmits, defineProps, ref } from 'vue'
// import moment from 'moment'

import useVisible from '@compos/use-visible'

import { ElUpload, ElMessage } from 'element-plus'

const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  query: {
    type: Object
  },
  productType: {
    type: [Number, String]
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

const uploadRef = ref()
const uploadLoading = ref(false)
const form = ref({})

function showHook() {
  form.value = {
    areaId: props.query.areaId,
    // booleanSaveTask: true,
    productType: props.productType
  }
}

function handleExceed(files, fileList) {
  ElMessage.warning(`当前限制选择 1 个文件`)
}

async function handleRequest(file) {
  try {
    console.log(file)
    uploadLoading.value = true
    const fileObj = file.file
    const formData = new FormData()
    formData.append('file', fileObj)
    for (const key in props.data) {
      formData.append(key, props.data[key])
    }

    const res = await taskImport(formData, form.value)
    console.log(res)
    handleClose()
  } catch (error) {
    console.log(error)
  } finally {
    uploadLoading.value = false
  }
}

function saveIt() {
  uploadRef.value?.submit()
}
</script>
