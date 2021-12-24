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
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
      <el-form-item label="名称" prop="name" v-if="form.sequenceType !== typeEnum.MACHINE_PART.V">
        {{ form.name }}
      </el-form-item>
      <el-form-item label="工序" prop="processId">
        <common-select
          v-model="form.processId"
          :options="form.processOption || []"
          size="small"
          type="other"
          clearable
          style="width: 250px"
          placeholder="请选择工序"
        />
      </el-form-item>
      <template v-if="form.processId">
        <el-form-item
          :label="`单价(${wageQuotaTypeEnum.V[form.processOption[form.processId].wageQuotaType].unit})`"
          prop="price"
        >
          <el-input-number
            v-model="form.processOption[form.processId].price"
            placeholder="请输入单价"
            :precision="2"
            controls-position="right"
            style="width: 250px"
            @change="val=>form.price=val"
          >
          </el-input-number>
        </el-form-item>
      </template>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { processMaterialListTypeEnum as typeEnum, wageQuotaTypeEnum } from '@enum-ms/mes'

import { regForm } from '@compos/use-crud'

const formRef = ref()

const defaultForm = {
  id: undefined,
  processId: undefined
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  processId: [{ required: true, message: '请选择工序', trigger: 'change' }],
  price: [{ required: true, message: '请选择填写单价', trigger: 'blur' }]
}

CRUD.HOOK.beforeToCU = () => {
  form.processId = form.process[0].processId
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  const { productProcessId } = form.processOption[form.processId]
  form.value = Object.assign(form, { productProcessId })
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-group__append) {
  width: 100px;
  text-align: center;
}
</style>

<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
