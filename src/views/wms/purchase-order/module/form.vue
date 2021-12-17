<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.status.cu > CRUD.STATUS.NORMAL"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    :size="1000"
    custom-class="purchase-order-form"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
        提 交
      </common-button>
      <store-operation v-if="crud.status.add > CRUD.STATUS.NORMAL" type="crud" />
    </template>
    <template #content>
      <div class="main-content">
        <el-form ref="formRef" :model="form" :rules="rules" size="small" label-position="top" inline label-width="200px">
          <div class="form-content">
            <el-form-item class="el-form-item-1" label="采购订单号" prop="serialNumber">
              <el-input
                v-model.trim="form.serialNumber"
                placeholder="可输入采购订单号（不填写则自动生成）"
                :disabled="form.boolUsed"
                maxlength="30"
                size="small"
                class="input-underline"
              />
            </el-form-item>
            <el-form-item class="el-form-item-2" label="供货类型" prop="supplyType">
              <common-radio
                v-model="form.supplyType"
                :options="orderSupplyTypeEnum.ENUM"
                type="enum"
                :disabled="form.boolUsed"
                size="small"
              />
            </el-form-item>
            <el-form-item class="el-form-item-3" label="物料种类" prop="basicClass">
              <div class="flex-rss child-mr-10">
                <common-radio-button
                  v-model="form.purchaseType"
                  :options="baseMaterialTypeEnum.ENUM"
                  type="enum"
                  :disabled="form.boolUsed"
                  size="mini"
                  style="flex: none"
                />
                <basic-class-select
                  v-model="form.basicClass"
                  :type="form.purchaseType"
                  multiple
                  clearable
                  :disabled="form.boolUsed"
                  placeholder="请选择物料种类"
                  class="input-underline"
                  style="flex: auto"
                />
              </div>
            </el-form-item>

            <el-form-item label="选择项目" class="el-form-item-4" prop="projectIds">
              <project-cascader v-model="form.projectIds" clearable :disabled="form.boolUsed" multiple class="input-underline" />
            </el-form-item>
            <el-form-item class="el-form-item-20" prop="areaIds">
              <area-table-tree
                v-if="baseMaterialTypeEnum.MANUFACTURED.V === form.purchaseType && form.basicClass && isNotBlank(form.projectIds)"
                v-model:struc="form.strucAreaIds"
                v-model:encl="form.enclAreaIds"
                :show-struc="!!(form.basicClass & matClsEnum.STRUC_MANUFACTURED.V)"
                :show-encl="!!(form.basicClass & matClsEnum.ENCL_MANUFACTURED.V)"
                :project-ids="form.projectIds"
                :disabled="form.boolUsed"
                checkable
              />
            </el-form-item>

            <el-form-item class="el-form-item-6" label="申购单号" prop="requisitionsSN">
              <requisitions-sn-select
                v-model="form.requisitionsSN"
                :project-id="form.projectIds"
                :basic-class="form.basicClass"
                clearable
                multiple
                :disabled="form.boolUsed"
                :public-warehouse="isBlank(form.projectIds)"
                class="input-underline"
                placeholder="可选择申购单号"
              />
            </el-form-item>
            <el-form-item class="el-form-item-7" label="选择供应商" prop="supplierId">
              <supplier-select
                v-model="form.supplierId"
                :basic-class="form.basicClass"
                :type="form.purchaseType"
                :disabled="form.boolUsed"
                mode="contain"
                clearable
                filterable
                class="input-underline"
              />
            </el-form-item>
            <template v-if="form.supplyType == orderSupplyTypeEnum.SELF.V">
              <el-form-item class="el-form-item-8" label="合同量" prop="mete">
                <div class="input-underline flex-rss child-mr-10">
                  <el-input-number
                    v-model="form.mete"
                    placeholder="请填写合同量"
                    autocomplete="off"
                    :max="999999999999"
                    :precision="5"
                    :step="1"
                    :controls="false"
                    style="width: 100%"
                  />
                  <unit-select
                    v-model="form.meteUnit"
                    size="small"
                    :disabled="form.boolUsed"
                    clearable
                    filterable
                    style="width: 100px; flex: none"
                  />
                </div>
              </el-form-item>
              <el-form-item class="el-form-item-9" label="合同额（元）" prop="amount">
                <el-input-number
                  v-model="form.amount"
                  :max="999999999999"
                  :precision="2"
                  :step="1"
                  :controls="false"
                  placeholder="请填写合同额"
                  autocomplete="off"
                  class="input-underline"
                  style="width: 100%"
                />
              </el-form-item>
              <el-form-item class="el-form-item-10" label="发票及税率" prop="invoiceType">
                <invoice-type-select
                  class="input-underline"
                  v-model:invoiceType="form.invoiceType"
                  v-model:taxRate="form.taxRate"
                  :disabled="form.boolUsed"
                  :classification="form.basicClass"
                />
              </el-form-item>
              <el-form-item class="el-form-item-11" prop="weightMeasurementMode" label="计量方式">
                <common-radio
                  v-model="form.weightMeasurementMode"
                  :options="weightMeasurementModeEnum.ENUM"
                  type="enum"
                  :props="{ key: 'K', label: 'SL', value: 'V' }"
                  :disabled="form.boolUsed"
                  :size="'small'"
                />
              </el-form-item>
              <el-form-item class="el-form-item-12" label="提货方式" prop="pickUpMode">
                <common-radio
                  v-model="form.pickUpMode"
                  :options="pickUpModeEnum.ENUM"
                  :disabled-val="pickUpModeDisabled"
                  type="enum"
                  :disabled="form.boolUsed"
                  size="small"
                />
              </el-form-item>
              <el-form-item class="el-form-item-13" label="订单类型" prop="purchaseOrderPaymentMode">
                <common-radio
                  v-model="form.purchaseOrderPaymentMode"
                  :options="purchaseOrderPaymentModeEnum.ENUM"
                  type="enum"
                  :disabled="form.boolUsed"
                  :size="'small'"
                />
              </el-form-item>
            </template>
            <el-form-item class="el-form-item-14" prop="remark">
              <el-input v-model="form.remark" :rows="3" type="textarea" placeholder="备注" maxlength="1000" show-word-limit />
            </el-form-item>
            <el-form-item class="el-form-item-15" prop="attachments">
              <!-- :show-download="!!form.id" -->
              <upload-list
                show-download
                :file-classify="fileClassifyEnum.PURCHASE_ORDER_ATT.V"
                v-model:files="form.attachments"
                empty-text="暂未上传采购订单附件"
              />
            </el-form-item>
          </div>
        </el-form>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, watch, computed, nextTick } from 'vue'
import { matClsEnum } from '@enum-ms/classification'
import { orderSupplyTypeEnum, baseMaterialTypeEnum, pickUpModeEnum, purchaseOrderPaymentModeEnum } from '@enum-ms/wms'
import { weightMeasurementModeEnum, invoiceTypeEnum } from '@enum-ms/finance'
import { fileClassifyEnum } from '@enum-ms/file'
import { isNotBlank, isBlank } from '@/utils/data-type'

import { regForm } from '@compos/use-crud'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import unitSelect from '@comp-common/unit-select/index.vue'
import projectCascader from '@comp-base/project-cascader.vue'
import supplierSelect from '@comp-base/supplier-select/index.vue'
import basicClassSelect from '@/components-system/classification/basic-class-select.vue'
import invoiceTypeSelect from '@/components-system/base/invoice-type-select.vue'
import requisitionsSnSelect from '@/components-system/wms/requisitions-sn-select.vue'
import uploadList from '@/components/file-upload/UploadList.vue'
import areaTableTree from '@/components-system/branch-sub-items/outsourcing-area-table-tree.vue'
import StoreOperation from '@crud/STORE.operation.vue'

const defaultForm = {
  serialNumber: undefined, // 采购订单编号
  supplyType: orderSupplyTypeEnum.SELF.V, // 供货类型
  purchaseType: baseMaterialTypeEnum.RAW_MATERIAL.V, // 物料种类
  basicClass: null, // 物料类型
  projectIds: undefined, // 项目ids
  areaIds: null, // 区域
  strucAreaIds: [], // 构件区域
  enclAreaIds: [], // 围护区域
  requisitionsSN: undefined, // 申购单编号
  supplierId: undefined, // 供应商id
  mete: undefined, // 合同量
  amount: undefined, // 合同金额
  invoiceType: invoiceTypeEnum.SPECIAL.V, // 发票类型
  taxRate: undefined, // 税率
  weightMeasurementMode: weightMeasurementModeEnum.THEORY.V, // 重量计量方式
  pickUpMode: pickUpModeEnum.SELF.V, // 提货方式
  purchaseOrderPaymentMode: purchaseOrderPaymentModeEnum.ARRIVAL.V, // 订单类型
  remark: undefined, // 备注
  attachments: undefined, // 附件
  attachmentIds: undefined // 附件ids
}

const validateInvoiceType = (rule, value, callback) => {
  if (form.invoiceType || form.invoiceType === 0) {
    if (form.invoiceType === invoiceTypeEnum.SPECIAL.V && !form.taxRate) {
      callback(new Error('请选择税率'))
      return
    } else {
      callback()
    }
  } else {
    callback(new Error('请选择发票及税率'))
    return
  }
}

const validateArea = (rule, value, callback) => {
  if (form.basicClass & matClsEnum.STRUC_MANUFACTURED.V && isBlank(form.strucAreaIds)) {
    callback(new Error('请选择构件区域'))
    return
  }
  if (form.basicClass & matClsEnum.ENCL_MANUFACTURED.V && isBlank(form.enclAreaIds)) {
    callback(new Error('请选择围护区域'))
    return
  }
  callback()
}

// 基础校验
const baseRules = {
  serialNumber: [{ max: 30, message: '订单编号长度不可超过30位', trigger: 'blur' }],
  supplyType: [{ required: true, message: '请选择供货类型', trigger: 'change' }],
  purchaseType: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  basicClass: [{ required: true, message: '请选择物料类型', trigger: 'change' }],
  supplierId: [{ required: true, message: '请选择供应商', trigger: 'change' }]
}

// 自采物料校验
const selfRules = {
  weightMeasurementMode: [{ required: true, message: '请选择计量方式', trigger: 'change' }],
  pickUpMode: [{ required: true, message: '请选择提货方式', trigger: 'change' }],
  purchaseOrderPaymentMode: [{ required: true, message: '请选择订单类型', trigger: 'change' }],
  invoiceType: [{ required: true, validator: validateInvoiceType, trigger: 'change' }],
  taxRate: [{ max: 2, message: '请输入税率', trigger: 'blur' }],
  mete: [{ required: true, message: '请填写合同量', trigger: 'blur' }],
  amount: [{ required: true, message: '请填写合同额', trigger: 'blur' }]
}

// 甲供校验
const partyARules = {
  projectIds: [{ required: true, message: '请选择项目', trigger: 'change' }]
}

const rawMatRules = {}

// 制成品校验
const manufRules = {
  projectIds: [{ required: true, message: '请选择项目', trigger: 'change' }],
  areaIds: [
    { validator: validateArea, trigger: 'change' }
    // { required: true, message: '请选择区域', trigger: 'change' }
  ]
}

const rules = computed(() => {
  const r = Object.assign({}, baseRules)
  // const oldFields = rules.value ? Object.keys(rules.value) : []
  if (form.purchaseType === baseMaterialTypeEnum.RAW_MATERIAL.V) {
    Object.assign(r, rawMatRules)
  }
  if (form.purchaseType === baseMaterialTypeEnum.MANUFACTURED.V) {
    Object.assign(r, manufRules)
  }
  if (form.supplyType === orderSupplyTypeEnum.SELF.V) {
    Object.assign(r, selfRules)
  }
  if (form.supplyType === orderSupplyTypeEnum.PARTY_A.V) {
    Object.assign(r, partyARules)
  }
  // const newFields = Object.keys(r)
  // const clearFields = oldFields.filter(v => !newFields.includes(v))
  // 清除被删除的rules
  nextTick(() => {
    // 清除所有校验，rules
    formRef.value && formRef.value.clearValidate()
  })
  return r
})

const formRef = ref()
const pickUpModeDisabled = ref([])

const { CRUD, crud, form } = regForm(defaultForm, formRef)

useWatchFormValidate(formRef, form, [['areaIds', ['purchaseType', 'basicClass', 'strucAreaIds', 'enclAreaIds']]])

watch(
  () => form.basicClass,
  (bc) => {
    if (bc & matClsEnum.GAS.V) {
      // 气体只能选择到厂
      form.pickUpMode = pickUpModeEnum.SUPPLIER.V
      pickUpModeDisabled.value = [pickUpModeEnum.SELF.V]
    } else {
      pickUpModeDisabled.value = []
    }
  }
)

// 初始化表单
CRUD.HOOK.afterToAdd = () => {}

CRUD.HOOK.beforeToEdit = (crud, form) => {
  if (isNotBlank(form.project)) {
    form.projectIds = form.project.map((v) => v.id)
  }
}

// 表单提交数据清理
crud.submitFormFormat = (form) => {
  form.attachmentIds = form.attachments ? form.attachments.map((v) => v.id) : undefined
  return form
}
</script>

<style lang="scss" scoped>
.main-content {
  ::v-deep(.input-underline input) {
    text-align: left;
  }
}

.form-content {
  width: 750px;
  display: grid;
  grid-template-columns: repeat(12, 8.33%);
  grid-template-rows: auto;
  grid-column-gap: 20px;
  grid-auto-flow: row;
  grid-template-areas:
    'a a a a a a . . . . . .'
    'c c c c c c b b b b b b'
    'd d d d d d d d d d d d'
    'z z z z z z z z z z z z'
    'f f f f f f g g g g g g'
    'h h h h i i i i j j j j'
    'k k k k l l l m m m m m'
    'n n n n n n n n n n n n'
    'o o o o o o o o o o o o';
  .el-form-item {
    margin-bottom: 20px;
  }
  > .el-form-item-1 {
    grid-area: a;
  }
  > .el-form-item-2 {
    grid-area: b;
  }
  > .el-form-item-3 {
    grid-area: c;
  }
  > .el-form-item-4 {
    grid-area: d;
  }
  > .el-form-item-5 {
    grid-area: e;
  }
  > .el-form-item-6 {
    grid-area: f;
  }
  > .el-form-item-7 {
    grid-area: g;
  }
  > .el-form-item-8 {
    grid-area: h;
  }
  > .el-form-item-9 {
    grid-area: i;
  }
  > .el-form-item-10 {
    grid-area: j;
  }
  > .el-form-item-11 {
    grid-area: k;
  }
  > .el-form-item-12 {
    grid-area: l;
  }
  > .el-form-item-13 {
    grid-area: m;
  }

  > .el-form-item-14 {
    grid-area: n;
  }

  > .el-form-item-15 {
    grid-area: o;
  }
  > .el-form-item-20 {
    grid-area: z;
  }
}
</style>
