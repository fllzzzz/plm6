<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    :title="'文件上传'"
    width="550px"
  >
    <template #titleAfter>
      <span style="color:red;font-size:12px;">*导入文件类型仅支持zip,pdf,jpg,jpeg,png格式</span>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="150px">
      <el-form-item label="选择阶段" prop="constructionStageType">
        <common-select
          v-model="form.constructionStageType"
          :options="constructionEnum.ENUM"
          type="enum"
          size="small"
          placeholder="选择阶段"
          style="width:260px"
        />
      </el-form-item>
      <el-form-item label="文件名">
        <el-input
          v-model.trim="form.fileName"
          placeholder="文件名"
          :maxlength="20"
          style="width: 260px;"
        />
      </el-form-item>
      <el-form-item label="上传">
        <upload-btn
          ref="uploadRef"
          :upload-fun="crudApi.add"
          :data="carryParam"
          :btn-name="'文件上传'"
          :btn-type="'warning'"
          :btn-size="'mini'"
          :icon="'el-icon-upload'"
          :accept="'.zip,.pdf,.jpg,.jpeg,.png'"
          class="filter-item"
          @success="handleSuccess"
          tip=".zip,.pdf,.jpg,.jpeg,.png"
          :disabled="!form.constructionStageType"
          :limit="1"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { defineProps, defineEmits, computed, ref, watch, nextTick } from 'vue'

import { constructionEnum } from '@enum-ms/project'

import useVisible from '@compos/use-visible'
import uploadBtn from './upload-btn'
import crudApi from '@/api/project-manage/data-manage/construction-data'

const props = defineProps({
  globalProject: {
    type: Object,
    default: () => {}
  },
  modelValue: {
    type: Boolean,
    require: true
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const defaultForm = {
  projectType: undefined,
  fileName: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()
const uploadRef = ref()
const rules = {
  constructionStageType: { required: true, message: '请选择阶段', trigger: 'change' },
  fileName: { required: true, message: '请输入文件命名', trigger: 'blur' }
}
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => visible.value,
  (val) => {
    if (val) {
      form.value.fileName = undefined
      form.value.constructionStageType = undefined
      if (formRef.value) {
        nextTick(() => {
          formRef.value.clearValidate()
        })
      }
    }
  },
  { deep: true, immediate: true }
)

const carryParam = computed(() => {
  return { projectId: props.globalProject.id, constructionStageType: form.value.constructionStageType, fileName: form.value.fileName }
})

function handleSuccess() {
  emit('success')
  handleClose()
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
