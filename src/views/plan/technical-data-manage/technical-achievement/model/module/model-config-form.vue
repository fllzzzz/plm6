<template>
  <common-dialog title="模型配置" v-model="dialogVisible" width="450px" :before-close="handleClose">
    <template #titleRight>
      <common-button :loading="saveLoading" size="mini" type="primary" @click="saveIt">保存</common-button>
    </template>
    <el-form ref="formRef" size="small" :model="form" :rules="rules" label-width="120px">
      <el-form-item label="所属项目">
        <span>{{ info.projectName }}</span>
      </el-form-item>
      <el-form-item label="单体">
        <span>{{ info?.name }}</span>
      </el-form-item>
      <el-form-item label="上传模式" prop="importMode">
        <common-radio v-model="form.importMode" :options="modelImportModeEnum.ENUM" type="enum" />
      </el-form-item>
      <el-form-item label="模型版本号" prop="edition">
        <common-select
          v-model="form.edition"
          :options="bimTeklaEditionEnum.ENUM"
          type="enum"
          size="small"
          style="width: 200px;"
          placeholder="Tekla版本"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { modelConfig } from '@/api/bim/model'
import { ElNotification } from 'element-plus'
import { defineEmits, defineProps, ref } from 'vue'

import { bimTeklaEditionEnum, modelImportModeEnum } from '@enum-ms/bim'

import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  },
  modelType: {
    type: String,
    default: 'monomer'
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

const formRef = ref()
const form = ref({
  importMode: modelImportModeEnum.MONOMER.V,
  edition: undefined
})
const saveLoading = ref(false)

const rules = {
  importMode: [{ required: true, message: '请选择上传模式', trigger: 'change' }],
  edition: [{ required: true, message: '请选择模型版本号', trigger: 'change' }]
}

function showHook() {
  form.value = {
    importMode: props.info?.importMode || modelImportModeEnum.MONOMER.V,
    edition: props.info?.edition || undefined
  }
}

function saveIt() {
  formRef.value.validate(async (valid) => {
    if (!valid) {
      return
    }
    try {
      saveLoading.value = true
      await modelConfig({ ...form.value, monomerId: props.info.id })
      ElNotification({
        title: '模型配置',
        type: 'success',
        message: '修改配置成功',
        duration: 2500
      })
      emit('success')
      handleClose()
    } catch (error) {
      console.log('模型配置失败', error)
    } finally {
      saveLoading.value = false
    }
  })
}
</script>
