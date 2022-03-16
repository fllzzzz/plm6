<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    :title="'文件替换'"
    width="550px"
  >
    <template #titleAfter>
      <span style="color:red;font-size:12px;">{{dataType===technicalDataTypeEnum.MACHINE_PART.V?'*替换文件仅支持DXF格式':'*替换文件仅支持DWG或PDF格式'}}</span>
    </template>
    <el-form ref="formRef" :model="form" size="small" label-width="150px">
      <el-form-item label="所属项目">
        <span class="project-name">{{ projectNameFormatter(currentProject) }}</span>
      </el-form-item>
      <el-form-item label="单体" v-if="currentMonomer && currentMonomer.name">
        <span>{{ currentMonomer.name }}</span>
      </el-form-item>
      <el-form-item label="编号">
        <span>{{ currentRow.serialNumber }}</span>
      </el-form-item>
      <el-form-item label="上传">
        <upload-btn
          ref="changeFileRef"
          :upload-fun="uploadCompile"
          :data="carryParam"
          :btn-name="'文件上传'"
          :btn-type="'warning'"
          :btn-size="'mini'"
          :data-type="dataType"
          :icon="'el-icon-upload'"
          :accept="dataType===technicalDataTypeEnum.MACHINE_PART.V?'.dxf':'.dwg,.pdf'"
          class="filter-item"
          @success="handleSuccess"
          :tip="dataType===technicalDataTypeEnum.MACHINE_PART.V?'.dxf':'.dwg,.pdf'"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { defineProps, defineEmits, computed, ref, watch } from 'vue'
import useVisible from '@compos/use-visible'
import uploadBtn from '../../../components/drawing-upload-btn'
import { uploadCompile } from '@/api/plan/technical-data-manage/deepen'
import { projectNameFormatter } from '@/utils/project'
import { technicalDataTypeEnum } from '@enum-ms/plan'

const props = defineProps({
  currentMonomer: {
    type: Object,
    default: () => {}
  },
  currentProject: {
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
  },
  productType: {
    type: [String, Number],
    default: undefined
  }
})

const defaultForm = {
  fileName: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()
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
  return { id: props.currentRow.id }
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
