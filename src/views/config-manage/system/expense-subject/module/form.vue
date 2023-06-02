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
      <el-form-item label="费用科目名称" prop="name">
        <el-input
          v-model.trim="form.name"
          placeholder="输入费用科目名称"
          class="filter-item"
          style="width: 330px"
          maxlength="32"
          size="small"
          clearable
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
  id: undefined,
  name: undefined
}

const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  name: [{ required: true, message: '请输入费用科目', trigger: 'blur' }]
}
</script>
