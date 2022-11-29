<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="550px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="170px">
      <el-form-item label="油漆类别">
        <span>{{ form.name }}</span>
      </el-form-item>
      <el-form-item label="计量方式" prop="measureUnit">
        <common-select
          v-model="form.measureUnit"
          :options="paintingMeasureUnitEnum.ENUM"
          type="enum"
          size="small"
          placeholder="请选择计量方式"
          style="width: 200px"
        />
      </el-form-item>
      <el-form-item label="标准涂装厚度（um）" prop="thickness">
        <common-input-number
          v-model="form.thickness"
          :min="0"
          :max="9999999"
          :controls="false"
          :step="5"
          size="mini"
          placeholder="标准涂装厚度"
          style="width: 200px"
        />
      </el-form-item>
      <el-form-item label="标准单价（元/㎡）" prop="unitPrice">
        <common-input-number
          v-model="form.unitPrice"
          :min="0"
          :max="9999999"
          :controls="false"
          :step="1"
          size="mini"
          placeholder="标准单价"
          style="width: 200px"
        />
      </el-form-item>
      <el-form-item label="超标准设定（um）" prop="outThickness">
        <common-input-number
          v-model="form.outThickness"
          :min="0"
          :max="9999999"
          :controls="false"
          :step="5"
          size="mini"
          placeholder="超标准设定"
          style="width: 200px"
        />
      </el-form-item>
      <el-form-item label="超标单价（元/㎡）" prop="outUnitPrice">
        <common-input-number
          v-model="form.outUnitPrice"
          :min="0"
          :max="9999999"
          :controls="false"
          :step="1"
          size="mini"
          placeholder="超标单价"
          style="width: 200px"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'

import { paintingMeasureUnitEnum } from '@enum-ms/mes'
import { regForm } from '@compos/use-crud'

const formRef = ref()
const defaultForm = {
  id: undefined
}

const { crud, form } = regForm(defaultForm, formRef)

const validateThickness = (rule, value, callback) => {
  if (value % 5 !== 0 || value === 0) {
    callback(new Error('厚度只能是5的倍数'))
    return
  }
  callback()
}

// const validateOutUnitPrice = (rule, value, callback) => {
//   if (form.outThickness && !value) {
//     callback(new Error('请填写超标单价'))
//     return
//   }
//   callback()
// }

const rules = {
  measureUnit: [{ required: true, message: '请选择计量方式', trigger: 'change' }],
  thickness: [
    { required: true, message: '请填写标准涂装厚度', trigger: 'blur' },
    { validator: validateThickness, trigger: 'blur' }
  ],
  unitPrice: [{ required: true, message: '请填写标准单价', trigger: 'blur' }],
  outThickness: [
    { required: true, message: '请填写超标准设定', trigger: 'blur' },
    { validator: validateThickness, trigger: 'blur' }
  ],
  outUnitPrice: [
    { required: true, message: '请填写超准单价', trigger: 'blur' }
    // { validator: validateOutUnitPrice, trigger: 'blur' }
  ]
}
</script>
