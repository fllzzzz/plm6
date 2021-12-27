<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="编辑项目模式"
    width="450px"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" type="primary" size="mini" @click="onSubmit">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
      <el-form-item label="项目模式" prop="projectMode">
        <div style="width: 260px">
          <common-select
            v-model="form.projectMode"
            :options="projectModeEnum.ENUM"
            type="enum"
            size="small"
            clearable
            class="filter-item"
            placeholder="项目模式"
            style="width: 250px"
          />
        </div>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, watch, defineProps, defineEmits } from 'vue'
import { projectModeEnum } from '@enum-ms/contract'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import useVisible from '@compos/use-visible'
import { edit } from '@/api/config/system-config/project-mode'
import { ElNotification } from 'element-plus'

const formRef = ref()
const defaultForm = {
  projectMode: undefined
}

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const submitLoading = ref(false)
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => props.modelValue,
  (val) => {
    if (val) {
      resetForm()
    }
  },
  { deep: true, immediate: true }
)

const rules = {
  projectMode: [{ required: true, message: '请选择项目模式', trigger: 'change' }]
}

function resetForm() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  useWatchFormValidate(formRef, form)
}

function handleSuccess() {
  ElNotification({ title: '提交成功', type: 'success' })
  emit('success')
  handleClose()
}

async function onSubmit(val) {
  submitLoading.value = true
  try {
    await formRef.value.validate()
    await edit(form.value)
    handleSuccess()
  } catch (e) {
    console.log('项目模式修改', e)
  } finally {
    submitLoading.value = false
  }
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
