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
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="110px">
      <el-form-item label="类别">
        <span>{{ form.steelName }}</span>
      </el-form-item>
      <el-form-item label="单价（元）" prop="unitPrice">
        <common-input-number
          v-model="form.unitPrice"
          :min="0"
          :max="9999999"
          :controls="false"
          :step="1"
          size="mini"
          placeholder="单价"
          style="width: 200px"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'

import { regForm } from '@compos/use-crud'

const formRef = ref()
const defaultForm = {
  id: undefined
}

const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }]
}
</script>
