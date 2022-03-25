<template>
  <common-dialog title="导入模型" v-model="dialogVisible" width="450px" :before-close="handleClose">
    <el-form size="small" label-width="120px">
      <el-form-item label="所属项目">
        <span>{{ info.projectName }}</span>
      </el-form-item>
      <el-form-item label="单体">
        <span>{{ info?.name }}</span>
      </el-form-item>
      <el-form-item label="上传">
        <upload-btn
          :upload-fun="upload"
          :disabled="!uploadAble"
          :data="{ monomerId: info?.id }"
          :fileClassify="undefined"
          :accept="'.ifc'"
          success-msg="模型导入成功"
          :btn-name="`${info?.hasModelImport ? '替换模型' : '导入模型'}`"
          btn-type="warning"
          btn-size="small"
          class="filter-item"
          @success="handleSuccess"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { upload } from '@/api/bim/model'
import { ElNotification } from 'element-plus'
import { computed, defineEmits, defineProps } from 'vue'

import useVisible from '@compos/use-visible'
import uploadBtn from '@comp/file-upload/SingleFileUploadBtn'

const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

const uploadAble = computed(() => {
  return props.info?.id
})

function handleSuccess() {
  ElNotification({
    title: '上传成功',
    type: 'success',
    message: '模型导入成功',
    duration: 2500
  })
  emit('success')
  handleClose()
}
</script>
