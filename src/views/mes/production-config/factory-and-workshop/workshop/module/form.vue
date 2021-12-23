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
     <el-form-item label="车间名称" prop="name">
        <el-input
          v-model.trim="form.name"
          type="text"
          placeholder="请填写名称"
          style="width: 270px;"
        />
      </el-form-item>
      <!-- <el-form-item label="车间简称" prop="shortName">
        <el-input
          v-model.trim="form.shortName"
          type="text"
          placeholder="请填写简称"
          style="width: 270px;"
        />
      </el-form-item> -->
      <el-form-item label="排序" prop="sort">
        <el-input-number
          v-model.number="form.sort"
          :min="1"
          :max="999"
          :step="1"
          controls-position="right"
          style="width: 270px;"
        />
      </el-form-item>
      <el-form-item label="备注" prop="remark">
        <el-input
          v-model="form.remark"
          type="textarea"
          :autosize="{ minRows: 4, maxRows: 6}"
          placeholder="请填写备注"
          style="width: 320px;"
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
  factoryId: undefined,
  name: '',
  // shortName: '',
  sort: 1,
  remark: ''
}

const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  sort: [
    { required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }
  ],
  name: [
    { required: true, message: '请填写车间名称', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  ],
  // shortName: [
  //   { required: true, message: '请填写车间简称', trigger: 'blur' },
  //   { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  // ],
  remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }]
}

</script>

<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
