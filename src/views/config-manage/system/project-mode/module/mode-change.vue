<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="编辑项目模式"
    width="860px"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" type="primary" size="mini" @click="onSubmit">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
      <el-form-item label="项目模式" prop="projectMode">
        <div style="width: 260px">
          <el-radio-group v-model="form.projectMode">
            <template v-for="(item,index) in modeOption" :key="index">
              <el-radio :label="projectModeEnum.STRUCTURE.V" v-if="item.V===projectModeEnum.STRUCTURE.V">简约模式 <span style="color:red;">（简约模式下，只对构件生产过程进行管理）</span></el-radio>
              <el-radio :label="projectModeEnum.STRUCTURE_ASSEMBLE.V" v-if="item.V===projectModeEnum.STRUCTURE_ASSEMBLE.V">标准模式 <span style="color:red;">（标准模式下，可将清单中构件和零件拆分为二，生成构件清单和零件清单，对其进行分开排产和管理）</span></el-radio>
            </template>
          </el-radio-group>
        </div>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, watch, defineProps, defineEmits } from 'vue'
import { ElRadioGroup } from 'element-plus'

import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import useVisible from '@compos/use-visible'
import { modeList, modeData, edit } from '@/api/config/system-config/project-mode'
import { ElNotification } from 'element-plus'
import { projectModeEnum } from '@enum-ms/contract'

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

const originData = [
  { L: '简约模式', K: 'STRUCTURE', V: 1 << 0 },
  { L: '标准模式', K: 'STRUCTURE_ASSEMBLE', V: 1 << 1 }
]
const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const submitLoading = ref(false)
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => props.modelValue,
  (val) => {
    if (val) {
      resetForm()
      getProjectMode()
    }
  },
  { deep: true, immediate: true }
)

const modeOption = ref([])
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

async function getProjectMode() {
  modeOption.value = []
  try {
    const data = await modeData()
    const { content } = await modeList()
    if (content && content.length > 0) {
      content.forEach(v => {
        if (originData.findIndex(k => k.V === v) > -1) {
          modeOption.value.push(originData.find(k => k.V === v))
        }
      })
    }
    form.value.projectMode = data.projectMode
  } catch (e) {
    console.log('获取项目模式', e)
  }
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
