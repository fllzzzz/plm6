<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    title="单件修正"
    width="500px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
      <el-form-item label="编号">
        {{ form.serialNumber }}
      </el-form-item>
      <el-form-item label="名称">
        {{ form.name}}
      </el-form-item>
      <el-form-item label="截面定义" prop="newSpecPrefix">
        <common-select
          v-model="form.newSpecPrefix"
          :options="specList"
          filterable
          clearable
          type="other"
          :dataStructure="{ key: 'id', label: 'value', value: 'id' }"
          placeholder="定义"
          style="width: 220px;"
        />
      </el-form-item>
      <el-form-item label="规格" prop="newSpecSection">
        <el-input
          v-model.trim="form.newSpecSection"
          type="text"
          :maxlength="30"
          placeholder="例如:400*200*10*12"
          style="width: 220px"/>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, inject } from 'vue'
import { regForm } from '@compos/use-crud'

const formRef = ref()
const defaultForm = {
  id: undefined,
  newSpecPrefix: undefined,
  newSpecSection: undefined
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)

const specList = inject('specList')
const rules = {
  newSpecPrefix: [
    { required: true, message: '请选择截面定义', trigger: 'change' }
  ],
  newSpecSection: [
    { required: true, message: '请填写规格', trigger: 'blur' },
    { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' }
  ]
}
CRUD.HOOK.beforeSubmit = () => {
  form.list = []
  form.list.push({
    id: form.id,
    newSpecPrefix: form.newSpecPrefix,
    newSpecSection: form.newSpecSection
  })
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.table-form {
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
  }
}
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
