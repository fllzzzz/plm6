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
        <el-form ref="formRef" :model="form" :rules="rules" size="small" label-position="top" inline label-width="80px">
          <el-form-item label="采购订单号" prop="orderNo">
            <div class="input-underline">
              <el-input
                v-model.trim="form.orderNo"
                placeholder="可输入采购订单号（不填写则自动生成）"
                :disabled="isUsed"
                maxlength="60"
                size="small"
                style="width: 350px"
              />
            </div>
          </el-form-item>
          <el-form-item label="供货类型" prop="supplyType" class="form-label-require">
            <common-radio v-model="form.supplyType" :options="orderSupplyTypeEnum.ENUM" type="enum" :disabled="isUsed" size="small" style="width:00px" />
          </el-form-item>
          <div class="form-row">
            <!-- <el-form-item label="物料种类" prop="basicClass">
              <div class="input-underline" style="min-width: 250px">
                <basic-class-select
                  v-model="form.basicClass"
                  :type=""
                  clearable
                  :disabled="isUsed"
                  placeholder="请选择物料种类"
                />
              </div>
            </el-form-item> -->
          </div>
          <el-form-item
            label="选择项目"
            prop="projectId"
            :class="form.supplyType === orderSupplyTypeEnum.PARTY_A.V ? 'form-label-require' : ''"
          >
            <div class="input-underline">
              <project-cascader
                v-model="form.projectId"
                style="width: 350px"
                clearable
                :disabled="isUsed"
                :multiple="true"
                :initial="false"
              />
            </div>
          </el-form-item>
          <el-form-item label="申购单号" prop="purchaseIds">
            <div class="input-underline">

              <!-- <purchase-no-select
                v-model="purchaseIds"
                :project-id="form.projectId"
                :basic-set-class="form.basicClass"
                clearable
                multiple
                :default="false"
                :disabled="isUsed"
                value-type="value"
                placeholder="请选择申购单号"
                style="width: 350px"
              >
                <template v-slot:view="{ data }">
                  <common-button icon="el-icon-view" type="info" size="mini" @click.stop="showPurchaseNoDetail(data)" />
                </template>
              </purchase-no-select> -->
            </div>
          </el-form-item>
          <template v-if="form.supplyType == orderSupplyTypeEnum.SELF.V">
            <el-form-item label="合同量" prop="totalMete">
              <div class="input-underline">
                <el-input v-model.trim="form.totalMete" style="width: 200px" placeholder="请填写合同量" autocomplete="off" />
                <el-select
                  v-model="form.meteUnit"
                  filterable
                  placeholder="请选择单位"
                  style="width: 150px"
                  @change="validateFieldTotalMete"
                >
                  <el-option v-for="item in dict.unit" :key="item.id" :label="item.label" :value="item.label" />
                </el-select>
              </div>
            </el-form-item>
            <div class="form-row">
              <el-form-item label="合同额（元）" prop="contractAmount">
                <div class="input-underline" style="min-width: 250px">
                  <el-input-number
                    v-model="form.contractAmount"
                    :max="999999999999"
                    :precision="$DP.YUAN"
                    :step="100"
                    :controls="false"
                    style="width: 200px"
                    placeholder="请填写合同额"
                    autocomplete="off"
                  />
                </div>
              </el-form-item>
              <el-form-item label="选择订单税率" prop="invoiceType">
                <div class="input-underline" style="min-width: 250px">
                  <invoice-type-select v-model:invoiceType="form.invoiceType" v-model:taxRate="form.taxRate" :disabled="isUsed" />
                </div>
              </el-form-item>
            </div>
            <el-form-item
              prop="measurementType"
              label="计量方式"
              :class="form.basicClass == STEEL_ENUM ? 'form-label-require' : ''"
            >
              <common-radio
                v-model="form.measurementType"
                :options="settlementTypeEnum"
                type="enum"
                :props="{ key: 'K', label: 'SL', value: 'V' }"
                :disabled="form.basicClass !== STEEL_ENUM || isUsed"
                :size="'small'"
              />
            </el-form-item>
            <div class="form-row">
              <el-form-item label="选择供应商" prop="supplierId">
                <div class="input-underline" style="min-width: 250px">
                  <supplier-select
                    ref="supplier"
                    v-model="form.supplierId"
                    :classification="classification"
                    :disabled="isUsed"
                    @change="supplierChange"
                  />
                </div>
              </el-form-item>
              <el-form-item label="提货方式" prop="transportType">
                <common-radio
                  v-model="form.transportType"
                  :options="pickUpModeEnum"
                  :disabled-val="pickUpModeDisabled"
                  type="enum"
                  :disabled="isUsed"
                  size="small"
                />
              </el-form-item>
            </div>
            <el-form-item label="订单类型" prop="type">
              <common-radio v-model="form.type" :options="arrivalModeEnum" type="enum" :disabled="isUsed" :size="'small'" />
            </el-form-item>
          </template>
        </el-form>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref } from 'vue'
import { STEEL_ENUM } from '@/settings/config'
import { orderSupplyTypeEnum } from '@enum-ms/wms'
import { isNotBlank } from '@/utils/data-type'

import { regForm } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import projectCascader from '@comp-base/project-cascader.vue'
import supplierSelect from '@comp-base/supplier-select.vue'
import invoiceTypeSelect from '@/components-system/base/invoice-type-select.vue'

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
// .spec-main-content {
//   padding: 10px 20px 00px 20px;
//   ::v-deep(.el-input-number .el-input__inner) {
//     text-align: left;
//   }
//   ::v-deep(.common-button--mini) {
//     min-height:20px;
//   }
// }
.main-content {
  ::v-deep(.el-form--inline .el-form-item){
    margin-right: 60px;
  }
}
</style>
