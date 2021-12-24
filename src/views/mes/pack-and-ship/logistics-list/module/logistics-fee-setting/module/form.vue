<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="520px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="110px">
      <el-form-item label="选择项目" prop="projectId">
        <project-cascader class="input-underline" v-model="form.projectId" style="width: 300px" clearable />
      </el-form-item>
      <el-form-item label="选择物流公司" prop="supplierId">
        <supplier-select class="input-underline" v-model="form.supplierId" style="width: 300px" />
      </el-form-item>
      <el-form-item label="计价方式" prop="priceType">
        <common-radio-button class="filter-item" v-model="form.priceType" :options="logisticsPriceTypeEnum.ENUM" type="enum" size="small" />
      </el-form-item>
      <el-form-item label="填写单价" prop="price">
        <el-input-number
          v-model="form.price"
          :max="999999999999"
          class="input-underline"
          :precision="DP.YUAN"
          :step="100"
          :controls="false"
          style="width: 100px"
          placeholder="请填写单价"
          autocomplete="off"
        />
        <span>{{ logisticsPriceTypeEnum.V[form.priceType].unit }}</span>
      </el-form-item>
      <el-form-item label="是否含税" prop="boolContainTaxEnum" class="form-label-require">
        <el-checkbox v-model="form.boolContainTaxEnum" label="含税"></el-checkbox>
      </el-form-item>
      <el-form-item label="税率" prop="tax" v-show="form.boolContainTaxEnum" class="form-label-require">
        <el-input-number
          v-model="form.tax"
          :max="999999999999"
          class="input-underline"
          :precision="DP.YUAN"
          :step="100"
          :controls="false"
          style="width: 100px"
          placeholder="请填写税率"
          autocomplete="off"
        />
        <span>%</span>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'

import { logisticsPriceTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'

import { regForm } from '@compos/use-crud'
import projectCascader from '@comp-base/project-cascader.vue'
import supplierSelect from '@comp-base/supplier-select/index.vue'

const formRef = ref()
const defaultForm = {
  id: undefined,
  projectId: undefined,
  supplierId: undefined,
  priceType: logisticsPriceTypeEnum.WEIGHT.V,
  boolContainTaxEnum: false,
  price: undefined,
  tax: undefined
}

const { crud, form } = regForm(defaultForm, formRef)

const validateTax = (rule, value, callback) => {
  if (form.boolContainTaxEnum) {
    if (!value) {
      callback(new Error('请填写税率'))
    }
  }
  callback()
}

const rules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  supplierId: [{ required: true, message: '请选择物流公司', trigger: 'change' }],
  priceType: [{ required: true, message: '请选择计价方式', trigger: 'change' }],
  price: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  tax: [{ validator: validateTax, trigger: 'blur' }]
}
</script>
