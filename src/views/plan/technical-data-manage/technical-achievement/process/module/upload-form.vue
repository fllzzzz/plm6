<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    :title="currentRow.id?'文件替换':'文件上传'"
    width="550px"
  >
    <template #titleAfter>
      <span style="color:red;font-size:12px;">*导入格式为压缩包,文件类型仅支持DWG或PDF格式</span>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="150px">
      <el-form-item label="涉及构件类型">
        <span class="project-name">{{ projectNameFormatter(globalProject) }}</span>
      </el-form-item>
      <el-form-item label="上传">
        <upload-btn
          ref="changeFileRef"
          :upload-fun="currentRow.id ? uploadCompile : upload"
          :data="carryParam"
          :btn-name="'文件上传'"
          :btn-type="'warning'"
          :btn-size="'mini'"
          :data-type="dataType"
          :icon="'el-icon-upload'"
          :accept="'.doc,.pdf'"
          class="filter-item"
          @success="handleSuccess"
          tip=".doc,.pdf"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { defineProps, defineEmits, computed, ref, watch } from 'vue'
import useVisible from '@compos/use-visible'
import uploadBtn from '../../../components/drawing-upload-btn'
import { upload, uploadCompile } from '@/api/plan/technical-data-manage/deepen'
import { projectNameFormatter } from '@/utils/project'

const props = defineProps({
  currentMonomer: {
    type: Object,
    default: () => {}
  },
  globalProject: {
    type: Object,
    default: () => {}
  },
  dataType: {
    type: [String, Number],
    default: undefined
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
  fileName: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()
const rules = {
}
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => visible.value,
  (val) => {
    if (val) {
      form.value.fileName = undefined
    }
  },
  { deep: true, immediate: true }
)

const carryParam = computed(() => {
  return {}
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
