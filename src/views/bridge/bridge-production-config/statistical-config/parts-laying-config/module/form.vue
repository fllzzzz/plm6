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
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px" label-position="right">
      <el-form-item label="截面类型" prop="specPrefix">
        <common-select
          v-model="form.specPrefix"
          :options="partKeyWordEnum.ENUM"
          clearable
          type="enum"
          :disabled="isEdit"
          placeholder="截面类型"
          style="width: 100%"
          class="input-underline"
        />
      </el-form-item>
      <el-form-item label="下料方式" prop="cutConfigId">
        <bridge-cut-config-select
          placeholder="下料方式"
          v-model="form.cutConfigId"
          clearable
          style="width: 100%"
          class="input-underline"
        />
      </el-form-item>
      <el-form-item label="板厚范围" prop="numerical">
        <common-input-number
          v-model="form.minNumerical"
          :step="1"
          :min="0"
          :precision="2"
          clearable
          :controls="false"
          size="mini"
          class="input-underline"
          placeholder="最小数值"
          style="width: 45%"
        />
        <span> ~ </span>
        <common-input-number
          v-model="form.maxNumerical"
          :step="1"
          :precision="2"
          class="input-underline"
          :controls="false"
          size="mini"
          clearable
          placeholder="最大数值"
          style="width: 45%"
        />
      </el-form-item>
      <el-form-item label="计量方式" prop="wageQuotaType">
        <common-select
          v-model="form.wageQuotaType"
          :options="wageQuotaTypeEnum.ENUM"
          clearable
          type="enum"
          placeholder="计量方式"
          style="width: 100%"
          class="input-underline"
        />
      </el-form-item>
      <el-form-item label="单价" prop="unitPrice">
        <common-input-number
          v-model="form.unitPrice"
          :step="1"
          :min="0"
          :precision="2"
          clearable
          class="input-underline"
          :controls="false"
          size="mini"
          placeholder="单价"
          style="width: 100%"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, computed } from 'vue'
import { partKeyWordEnum, wageQuotaTypeEnum } from '@enum-ms/mes'
import { regForm } from '@compos/use-crud'
import bridgeCutConfigSelect from '@/components-system/base/bridge-cut-config-select.vue'

const formRef = ref()
const defaultForm = {
  id: undefined
}

const { crud, form } = regForm(defaultForm, formRef)

// 是否是编辑状态
const isEdit = computed(() => {
  return crud.status.edit > 0
})

const validateNumerical = (rule, value, callback) => {
  if (!form.minNumerical || !form.maxNumerical) {
    callback(new Error('请填写数值'))
  } else if (form.maxNumerical < form.minNumerical) {
    callback(new Error('最大数值不得小于最小数值'))
  }
  callback()
}

const rules = {
  specPrefix: [{ required: true, message: '请选择截面类型', trigger: 'change' }],
  cutConfigId: [{ required: true, message: '请选择下料方式', trigger: 'change' }],
  wageQuotaType: [{ required: true, message: '请选择计量方式', trigger: 'change' }],
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  numerical: [{ required: true, validator: validateNumerical, message: '请填写数值', trigger: 'blur' }]
}
</script>
