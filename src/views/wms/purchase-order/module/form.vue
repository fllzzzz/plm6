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
      <common-button :loading="crud.bStatus.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
        提 交
      </common-button>
    </template>
    <template #content>
      <div class="main-content" :style="maxHeightStyle">
        <el-form ref="formRef" :model="form" :rules="rules" size="small" label-position="top" inline label-width="200px">
          <div class="form-content">
            <el-form-item class="el-form-item-1" label="采购订单号" prop="orderNo">
              <el-input
                v-model.trim="form.orderNo"
                placeholder="可输入采购订单号（不填写则自动生成）"
                :disabled="isUsed"
                maxlength="50"
                size="small"
                class="input-underline"
              />
            </el-form-item>
            <el-form-item class="el-form-item-2" label="供货类型" prop="supplyType">
              <common-radio v-model="form.supplyType" :options="orderSupplyTypeEnum.ENUM" type="enum" :disabled="isUsed" size="small" />
            </el-form-item>
            <el-form-item class="el-form-item-3" label="物料种类" prop="basicClass">
              <div class="flex-rss child-mr-10">
                <common-radio-button
                  v-model="form.purchaseType"
                  :options="baseMaterialTypeEnum.ENUM"
                  type="enum"
                  :disabled="isUsed"
                  size="mini"
                  style="flex: none"
                />
                <basic-class-select
                  v-model="form.basicClass"
                  :type="form.purchaseType"
                  multiple
                  clearable
                  :disabled="isUsed"
                  placeholder="请选择物料种类"
                  class="input-underline"
                  style="flex: auto"
                />
              </div>
            </el-form-item>

            <el-form-item label="选择项目" class="el-form-item-4" prop="projectId">
              <project-cascader
                v-model="form.projectId"
                clearable
                :disabled="isUsed"
                :multiple="true"
                :initial="false"
                class="input-underline"
              />
            </el-form-item>
            <el-form-item label="选择单体/区域" class="el-form-item-5" prop="projectId">
              <!-- <project-cascader
                v-model="form.projectId"
                style="width: 350px"
                clearable
                :disabled="isUsed"
                :multiple="true"
                :initial="false"
                class="input-underline"
              /> -->
            </el-form-item>

            <el-form-item class="el-form-item-6" label="申购单号" prop="purchaseIds">
              <requisitions-sn-select
                v-model="form.purchaseIds"
                :project-id="form.projectId"
                :basic-class="form.basicClass"
                clearable
                multiple
                :disabled="isUsed"
                :public-warehouse="isBlank(form.projectId)"
                class="input-underline"
                placeholder="可选择申购单号"
              />
            </el-form-item>
            <el-form-item class="el-form-item-7" label="选择供应商" prop="supplierId">
              <supplier-select
                v-model="form.supplierId"
                :basicClass="form.basicClass"
                :type="form.purchaseType"
                :disabled="isUsed"
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
                    :precision="2"
                    :step="1"
                    :controls="false"
                    style="width: 100%"
                  />
                  <unit-select v-model="form.meteUnit" size="small" :disabled="isUsed" clearable filterable style="width: 100px; flex: none" />
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
              <el-form-item class="el-form-item-10" label="选择订单税率" prop="invoiceType">
                <invoice-type-select
                  class="input-underline"
                  v-model:invoiceType="form.invoiceType"
                  v-model:taxRate="form.taxRate"
                  :disabled="isUsed"
                  :classification="form.basicClass"
                />
              </el-form-item>
              <el-form-item class="el-form-item-11" prop="weightMeasurementMode" label="计量方式">
                <common-radio
                  v-model="form.weightMeasurementMode"
                  :options="weightMeasurementModeEnum.ENUM"
                  type="enum"
                  :props="{ key: 'K', label: 'SL', value: 'V' }"
                  :disabled="isUsed"
                  :size="'small'"
                />
              </el-form-item>
              <el-form-item class="el-form-item-12" label="提货方式" prop="pickUpMode">
                <common-radio
                  v-model="form.pickUpMode"
                  :options="pickUpModeEnum.ENUM"
                  :disabled-val="pickUpModeDisabled"
                  type="enum"
                  :disabled="isUsed"
                  size="small"
                />
              </el-form-item>
              <el-form-item class="el-form-item-13" label="订单类型" prop="paymentMode">
                <common-radio
                  v-model="form.paymentMode"
                  :options="purchaseOrderPaymentModeEnum.ENUM"
                  type="enum"
                  :disabled="isUsed"
                  :size="'small'"
                />
              </el-form-item>
            </template>
          </div>
        </el-form>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, watch } from 'vue'
import { materialClassificationEnum } from '@enum-ms/classification'
import { orderSupplyTypeEnum, baseMaterialTypeEnum, pickUpModeEnum, purchaseOrderPaymentModeEnum } from '@enum-ms/wms'
import { weightMeasurementModeEnum } from '@/utils/enum/modules/finance'
import { isNotBlank, isBlank } from '@/utils/data-type'

import { regForm } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import unitSelect from '@comp-common/unit-select/index.vue'
import projectCascader from '@comp-base/project-cascader.vue'
import supplierSelect from '@comp-base/supplier-select.vue'
import basicClassSelect from '@/components-system/classification/basic-class-select.vue'
import invoiceTypeSelect from '@/components-system/base/invoice-type-select.vue'
import requisitionsSnSelect from '@/components-system/wms/requisitions-sn-select.vue'

const defaultForm = {
  name: '', // 规格名称
  classificationId: undefined // 科目id
}

const rules = {
  name: [{ required: true, message: '请输入规格名称', trigger: 'blur' }]
  // boolWeightMean: [{ required: true, message: '请选择是否参加加权平均', trigger: 'change' }]
}

const drawerRef = ref()
const formRef = ref()
const isUsed = ref(false) // 采购单是否已被使用，使用后部分信息无法修改
const pickUpModeDisabled = ref([])

const { maxHeightStyle } = useMaxHeight(
  {
    mainBox: '.purchase-order-form',
    extraBox: '#el-drawer__title',
    wrapperBox: ['.el-drawer__body', 'main-content'],
    navbar: false
  },
  () => drawerRef.value.loaded
)

const { CRUD, crud, form } = regForm(defaultForm, formRef)

watch(
  () => form.basicClass,
  (bc) => {
    if (bc & materialClassificationEnum.GAS.V) {
      // 气体只能选择到场
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
  isUsed.value = form.isUsed
  if (isNotBlank(form.project)) {
    form.projectId = form.project.map((v) => v.id)
  }
}

// 表单提交数据清理
crud.submitFormFormat = (form) => {
  // form.classificationId = currentNode.value.id
  return form
}
</script>

<style lang="scss" scoped>
.main-content {
  ::v-deep(.el-form--inline .el-form-item) {
    // margin-right: 60px;
  }

  ::v-deep(.el-form-item) {
    // width: 400px;
  }
  ::v-deep(.input-underline input) {
    text-align: left;
  }
}

.form-content {
  width: 90%;
  display: grid;
  grid-template-columns: repeat(6, 16.66%);
  grid-template-rows: auto;
  // grid-row-gap: 20px;
  grid-column-gap: 20px;
  grid-auto-flow: row;
  grid-template-areas:
    'a a a b b b'
    'c c c . . .'
    'd d d e e e'
    'f f f g g g'
    'h h i i j j'
    'k k l l . .'
    'm m m . . .';
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
}
</style>
