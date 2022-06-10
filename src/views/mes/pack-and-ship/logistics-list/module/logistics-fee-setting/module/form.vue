<template>
  <common-drawer
    ref="drawerRef"
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    size="800"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="110px">
        <div class="table-top">
          <el-form-item label="选择项目" prop="projectId">
            <project-cascader class="input-underline" :disabled="form.boolUseEnum" v-model="form.projectId" style="width: 300px" clearable />
          </el-form-item>
          <el-form-item label="选择物流公司" prop="supplierId">
            <supplier-select
              class="input-underline"
              v-model="form.supplierId"
              :disabled="form.boolUseEnum"
              :type="supplierTypeEnum.LOGISTICS.V"
              style="width: 300px"
            />
          </el-form-item>
          <el-form-item label="计价方式" prop="priceType">
            <common-radio-button
              class="filter-item"
              v-model="form.priceType"
              :disabled="form.boolUseEnum"
              :options="logisticsPriceTypeEnum.ENUM"
              type="enum"
              size="small"
            />
          </el-form-item>
          <el-form-item label="基础价格" prop="price">
            <el-input-number
              v-model="form.price"
              :max="999999999999"
              class="input-underline"
              :precision="DP.YUAN"
              :step="100"
              :controls="false"
              style="width: 100px"
              placeholder="基础价格"
              autocomplete="off"
            />
            <span :class="form.priceType === logisticsPriceTypeEnum.WEIGHT.V ? 'blue':'orange'">{{ logisticsPriceTypeEnum.V[form.priceType].unit }}</span>
          </el-form-item>
          <el-form-item label="发票及税率" prop="invoiceTypeEnum" class="form-label-require">
            <invoice-type-select
              class="input-underline"
              v-model:invoiceType="form.invoiceTypeEnum"
              v-model:taxRate="form.tax"
              style="width: 360px"
              :classification="supplierClassEnum.LOGISTICS.V"
              :disabled="form.boolUseEnum"
              :default="form.id?false:true"
            />
          </el-form-item>
        </div>
        <common-table
          ref="detailRef"
          border
          :data="form.list"
          :max-height="maxHeight"
          style="width: 100%"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column prop="carModel" label="车型" align="center" />
          <el-table-column prop="priceType" label="计价方式" align="center" min-width="270">
            <template #header>
              <span style="margin-right:3px;vertical-align:-3px;">计价方式</span>
              <common-radio-button
                class="filter-item"
                v-model="tablePriceType"
                :options="logisticsPriceTypeEnum.ENUM"
                type="enum"
                size="mini"
                @change="priceTypeChange"
                :disabled="form.boolUseEnum"
              />
            </template>
            <template v-slot="scope">
              <common-radio
                class="filter-item"
                v-model="scope.row.priceType"
                :options="logisticsPriceTypeEnum.ENUM"
                :disabled="form.boolUseEnum"
                type="enum"
                size="small"
              />
            </template>
          </el-table-column>
          <el-table-column prop="price" label="价格" align="center" width="220">
            <template v-slot="scope">
                <el-input-number
                  v-model.number="scope.row.price"
                  :min="0"
                  :max="999999999999"
                  :step="100"
                  :precision="DP.YUAN"
                  placeholder="价格"
                  controls-position="right"
                  style="width:150px;margin-right:3px;"
                />
                <span :class="scope.row.priceType === logisticsPriceTypeEnum.WEIGHT.V ? 'blue':'orange'" >{{ logisticsPriceTypeEnum.V[scope.row.priceType].unit }}</span>
            </template>
          </el-table-column>
        </common-table>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref } from 'vue'
import { getCarModelConfig } from '@/api/config/mes/base'
import { getSupplierCarPrice } from '@/api/mes/pack-and-ship/logistics-list'

import { logisticsPriceTypeEnum } from '@enum-ms/mes'
import { supplierTypeEnum, supplierClassEnum } from '@/utils/enum/modules/supplier'
import { invoiceTypeEnum as financeInvoiceTypeEnum } from '@enum-ms/finance'
import { DP } from '@/settings/config'
import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'

import { regForm } from '@compos/use-crud'
import projectCascader from '@comp-base/project-cascader.vue'
import supplierSelect from '@comp-base/supplier-select/index.vue'
import invoiceTypeSelect from '@comp-base/invoice-type-select.vue'

const formRef = ref()
const drawerRef = ref()

const defaultForm = {
  id: undefined,
  projectId: undefined,
  supplierId: undefined,
  priceType: logisticsPriceTypeEnum.WEIGHT.V,
  // boolContainTaxEnum: false,
  boolUseEnum: false,
  price: undefined,
  tax: undefined,
  invoiceTypeEnum: undefined,
  list: []
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const tablePriceType = ref(logisticsPriceTypeEnum.WEIGHT.V)

const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.table-top'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    extraHeight: 120
  },
  () => drawerRef.value.loaded
)

const validateInvoiceType = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请选择发票类型'))
  } else {
    if (value !== financeInvoiceTypeEnum.RECEIPT.V) {
      if (!form.tax) {
        callback(new Error('请选择税率'))
      }
      callback()
    }
    callback()
  }
}

const rules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  supplierId: [{ required: true, message: '请选择物流公司', trigger: 'change' }],
  priceType: [{ required: true, message: '请选择计价方式', trigger: 'change' }],
  price: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  invoiceTypeEnum: [{ validator: validateInvoiceType, trigger: 'blur' }]
}

// 价格校验
const validatePrice = (value, row) => {
  if (row.priceType !== form.priceType) {
    if (!value) return false
    return true
  } else {
    return true
  }
}

const tableRules = {
  price: [{ validator: validatePrice, message: '请填写价格', trigger: 'change' }]
}
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

function priceTypeChange(val) {
  form.list.map(v => {
    v.priceType = val
  })
}

async function fetchModelData() {
  form.list = []
  try {
    const data = await getCarModelConfig()
    const supplierCarData = crud.status.edit === CRUD.STATUS.PREPARED ? await getSupplierCarPrice(form.id) : []
    if (data.carModels && data.carModels.length > 0) {
      for (let i = 0; i < data.carModels.length; i++) {
        if (supplierCarData.length > 0 && supplierCarData.findIndex(k => k.carModel === data.carModels[i]) > -1) {
          form.list.push(supplierCarData.find(k => k.carModel === data.carModels[i]))
        } else {
          form.list.push({
            carModel: data.carModels[i],
            price: undefined,
            priceType: form.priceType || logisticsPriceTypeEnum.WEIGHT.V,
            supplierPriceId: undefined
          })
        }
      }
    }
  } catch (error) {
    console.log('获取车型配置', error)
  }
}

CRUD.HOOK.afterToCU = (crud, form) => {
  tablePriceType.value = crud.status.edit === CRUD.STATUS.PREPARED ? crud.form.priceType : logisticsPriceTypeEnum.WEIGHT.V
  fetchModelData()
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  const { validResult, dealList } = tableValidate(crud.form.list)
  if (validResult) {
    crud.form.list = dealList
  } else {
    return validResult
  }
  crud.form.linkList = []
  crud.form.list.map(v => {
    if (v.price && v.price > 0) {
      v.supplierPriceId = crud.form.supplierPriceId
      crud.form.linkList.push(v)
    }
  })
}
</script>
<style lang="scss" scoped>
  .blue{
    color:#409eff;
  }
  .orange{
    color:#e6a23c;
  }
</style>
