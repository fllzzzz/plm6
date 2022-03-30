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
        <div style="display:flex;">
          <upload-btn
            :upload-fun="upload"
            :disabled="!uploadAble"
            :data="{ monomerId: info?.id, edition: edition }"
            :fileClassify="undefined"
            :accept="'.ifc'"
            success-msg="模型导入成功"
            :btn-name="`${info?.hasModelImport ? '替换模型' : '导入模型'}`"
            btn-type="warning"
            btn-size="small"
            @success="handleSuccess"
          />
          <common-select
            v-model="edition"
            :options="bimTeklaEditionEnum.ENUM"
            type="enum"
            size="small"
            style="width: 120px; margin-left: 5px"
            placeholder="Tekla版本"
          />
        </div>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { upload } from '@/api/bim/model'
import { ElNotification } from 'element-plus'
import { computed, defineEmits, defineProps, ref } from 'vue'

import { bimTeklaEditionEnum } from '@enum-ms/bim'

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
  return props.info?.id && edition.value
})

const edition = ref()

function handleSuccess() {
  ElNotification({
    title: '上传成功',
    type: 'success',
    message: '模型导入成功',
    duration: 2500
  })
  emit('success')
  edition.value = null
  handleClose()
}
</script>
