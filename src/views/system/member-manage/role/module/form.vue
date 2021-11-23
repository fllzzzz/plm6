<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="600px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="80px" :inline="true">
      <el-form-item label="角色名称" prop="name">
        <el-input v-model="form.name" style="width: 145px;" />
      </el-form-item>
      <el-form-item label="描述信息" prop="remark">
        <el-input v-model="form.remark" style="width: 380px;" rows="5" type="textarea" />
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
  name: undefined,
  remark: undefined
}
const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  name: [
    { required: true, message: '请填写名称', trigger: 'blur' }
  ]
}

</script>
<style lang="scss" scoped>
  ::v-deep(.el-input-number .el-input__inner) {
    text-align: left;
  }
</style>
