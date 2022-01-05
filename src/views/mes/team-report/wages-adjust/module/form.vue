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
      <el-form-item label="项目">
        {{ globalProject.name }}
      </el-form-item>
      <el-form-item label="单体" prop="monomerName">
        {{ form.monomerName }}
      </el-form-item>
      <el-form-item label="名称" prop="name">
        {{ form.name }}
      </el-form-item>
      <el-form-item label="工序" prop="processName">
        {{ form.processName }}
      </el-form-item>
      <el-form-item label="定额单价" prop="originPrice">
        {{ form.originPrice }}
      </el-form-item>
      <el-form-item label="调整单价" prop="price">
        <el-input-number
          v-model="form.price"
          placeholder="请输入单价"
          :precision="2"
          controls-position="right"
          oninput="value=value.replace(/[^0-9.]/g,'')"
          style="width: 250px"
        >
        </el-input-number>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { ElMessage } from 'element-plus'
import { mapGetters } from '@/store/lib'

import { judgeSameValue } from '@data-type/index'
import { regForm } from '@compos/use-crud'

const formRef = ref()
const defaultForm = {
  id: undefined
}

const { globalProject } = mapGetters(['globalProject'])

const { crud, CRUD, form } = regForm(defaultForm, formRef)

const rules = {}

CRUD.HOOK.beforeSubmit = () => {
  if (judgeSameValue(form.originPrice, form.price)) {
    ElMessage.warning('未修改')
    return false
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
