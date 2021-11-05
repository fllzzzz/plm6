<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :visible="crud.status.cu > 0"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    width="400px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" size="mini" type="primary" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="55px">
      <el-form-item label="国标" prop="name">
        <el-input v-model.trim="form.name" type="text" placeholder="" style="width: 300px" />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'

const defaultForm = {
  name: '' // 国标名称
}

const formRef = ref()
const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  name: [
    { required: true, message: '请填写国标', trigger: 'blur' },
    { max: 20, message: '不能超过20个字符', trigger: 'blur' }
  ]
}
</script>
