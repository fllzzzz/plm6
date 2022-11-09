<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="500px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="90px">
      <el-form-item label="切割方式" prop="name">
        <el-input v-model="form.name" :maxlength="8" placeholder="请填写切割方式" style="width: 270px" />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'

const formRef = ref()

const defaultForm = {
  id: undefined,
  name: undefined
}

const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  name: [
    { required: true, message: '请填写切割方式', trigger: 'blur' },
    { max: 8, message: '不能超过8个字符', trigger: 'blur' }
  ]
}
</script>
