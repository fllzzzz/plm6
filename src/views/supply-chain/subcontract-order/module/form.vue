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
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">提交</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px" label-position="right">
      <el-form-item label="订单编号" prop="serialNumber">
        <el-input
          v-model="form.serialNumber"
          type="text"
          maxlength="20"
          placeholder="请输入订单编号"
          style="width:200px;"
        />
      </el-form-item>
      <el-form-item label="签订日期" prop="signDate">
        <el-date-picker
          v-model="form.signDate"
          type="date"
          value-format="x"
          style="width: 200px;"
          placeholder="签订日期"
        />
      </el-form-item>
      <el-form-item label="项目" prop="projectId">
        <project-subcontract-select
          v-model="form.projectId"
          :defaultId="form.id?form.projectId:undefined"
          @change="projectChange"
        />
      </el-form-item>
      <el-form-item label="包含单体" prop="monomerIds">
        <monomer-select
          ref="monomerSelectRef"
          v-model="form.monomerIds"
          :project-id="form.projectId"
          multiple
          :default="false"
          :defaultValue="form.id?form.monomerIds:undefined"
          clearable
        />
      </el-form-item>
      <el-form-item label="分包单位" prop="supplierId">
        <supplier-select
          v-model="form.supplierId"
          :type="supplierTypeEnum.SUBCONTRACTING.V"
          clearable
          filterable
        />
      </el-form-item>
      <el-form-item label="分包类别" prop="subcontractClassId">
        <subcontractType v-model="form.subcontractClassId" />
      </el-form-item>
      <el-form-item label="合同额" prop="amount">
        <el-input-number
          v-show-thousand
          v-model="form.amount"
          :max="9999999999"
          :min="0"
          :step="100"
          :precision="DP.YUAN"
          controls-position="right"
          placeholder="合同额(元)"
          style="width:200px;"
        />
      </el-form-item>
      <el-form-item label="发票类型" prop="invoiceType">
        <common-select
          type="enum"
          size="small"
          v-model="form.invoiceType"
          :options="invoiceTypeEnum.ENUM"
          placeholder="选择发票类型"
          @change="invoiceTypeChange"
        />
      </el-form-item>
      <el-form-item label="税率" prop="taxRate" v-if="form.invoiceType !== invoiceTypeEnum.RECEIPT.V">
        <el-input-number
          v-model="form.taxRate"
          :step="1"
          :min="0"
          :max="100"
          :precision="0"
          :controls="false"
          placeholder="0-100"
        />%
      </el-form-item>
      <el-form-item label="备注" prop="remark">
        <el-input
          v-model="form.remark"
          type="textarea"
          style="width: 100%"
          maxlength="500"
          :autosize="{ minRows: 2, maxRows: 4 }"
          placeholder="请输入备注"
          show-word-limit
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'

import { regForm } from '@compos/use-crud'
import { supplierTypeEnum } from '@enum-ms/supplier'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { DP } from '@/settings/config'

import supplierSelect from '@comp-base/supplier-select/index.vue'
import projectSubcontractSelect from '@/components-system/project/project-subcontract-select.vue'
import monomerSelect from '@/components-system/plan/monomer-select'
import subcontractType from '@/components-system/project/subcontract-type-select'

const formRef = ref()
const defaultForm = {
  serialNumber: undefined,
  projectId: undefined,
  monomerIds: undefined,
  subcontractClassId: undefined,
  supplierId: undefined,
  amount: undefined,
  invoiceType: undefined,
  taxRate: undefined,
  remark: undefined,
  signDate: undefined
}

const { crud, form } = regForm(defaultForm, formRef)

const validateTaxRate = (rule, value, callback) => {
  if (form.invoiceType !== invoiceTypeEnum.RECEIPT.V) {
    if (!value) {
      callback(new Error('请填写税率'))
    }
  }
  callback()
}

const rules = {
  serialNumber: [{ required: true, message: '请输入订单编号', trigger: 'blur' }],
  signDate: [{ required: true, message: '请选择签订日期', trigger: 'change' }],
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  monomerIds: [{ required: true, message: '请选择单体', trigger: 'change' }],
  subcontractClassId: [{ required: true, message: '请选择分包类别', trigger: 'change' }],
  supplierId: [{ required: true, message: '请选择分包单位', trigger: 'change' }],
  amount: [{ required: true, message: '请输入合同额', trigger: 'change' }],
  invoiceType: [{ required: true, message: '请选择发票类型', trigger: 'change' }],
  taxRate: [{ required: true, validator: validateTaxRate, trigger: 'blur' }]
}

function invoiceTypeChange(val) {
  if (val === invoiceTypeEnum.RECEIPT.V) {
    form.taxRate = undefined
  }
}

function projectChange() {
  form.monomerIds = []
}

</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
