<template>
  <div class="inbound-application-header flex-rbc">
    <div>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-position="right" inline label-width="80px">
        <el-form-item label="订单号" prop="purchaseId" label-width="70px">
          <purchase-sn-select
            class="input-underline"
            v-model="form.purchaseId"
            v-model:info="purchaseOrderInfo"
            :basicClass="props.basicClass"
            @change="handlePurchaseIdChange"
            @info-change="handleOrderInfoChange"
            style="width: 300px"
          />
        </el-form-item>
        <el-form-item label="车牌号" prop="licensePlate" label-width="70px">
          <el-input class="input-underline" v-model="form.licensePlate" placeholder="请输入车牌号" style="width: 125px" />
        </el-form-item>
        <el-form-item
          v-if="orderInfo.weightMeasurementMode === weightMeasurementModeEnum.OVERWEIGHT.V"
          :label="`过磅重量(千克)`"
          label-width="120px"
          prop="loadingWeight"
        >
          <div class="input-underline">
            <el-input-number
              v-model="form.loadingWeight"
              style="width: 135px"
              :min="0"
              :max="9999999"
              :controls="false"
              :precision="3"
              :placeholder="`输入该车次重量`"
            />
          </div>
        </el-form-item>
      </el-form>
    </div>
    <div class="child-mr-10">
      <common-button type="primary" size="small" @click="openRequisitionsView">查看申购单</common-button>
      <el-tooltip content="请先选择订单号" :disabled="!!form.purchaseId" placement="bottom" effect="light">
        <excel-resolve-button
          icon="el-icon-upload2"
          btn-name="批量导入"
          btn-size="small"
          btn-type="success"
          :disabled="!form.purchaseId"
          @success="handleExcelSuccess"
        />
      </el-tooltip>
      <el-tooltip :content="`入库记录`" :show-after="1000" effect="light" placement="bottom">
        <common-button icon="el-icon-time" type="info" size="small" @click="toInboundRecord" />
      </el-tooltip>
    </div>
  </div>
</template>

<script setup>
import { defineProps, defineEmits, defineExpose, ref, computed, inject } from 'vue'
import { deepClone } from '@/utils/data-type'
import { weightMeasurementModeEnum } from '@enum-ms/finance'
import { patternLicensePlate } from '@/utils/validate/pattern'

import excelResolveButton from '@/components-system/common/excel-resolve-button/index.vue'
import purchaseSnSelect from '@/components-system/wms/purchase-sn-select/index.vue'

const emit = defineEmits(['update:purchaseId', 'purchase-order-change'])

const props = defineProps({
  purchaseId: {
    type: String
  },
  basicClass: {
    type: Number
  }
})

// const defaultForm = {}
const form = inject('form')

const rules = {
  purchaseId: [{ required: true, message: '请选择订单', trigger: 'change' }],
  licensePlate: [
    { required: true, message: '请填写车牌号', trigger: 'blur' },
    { pattern: patternLicensePlate, message: '请填写正确的车牌号', trigger: 'blur' }
  ],
  loadingWeight: [{ required: true, message: '请填写过磅重量', trigger: 'blur' }]
}

const formRef = ref()
// const form = ref(deepClone(defaultForm))
const purchaseOrderInfo = ref({})
const orderInfo = computed(() => {
  return purchaseOrderInfo.value || {}
})

// 采购订单id变更
function handlePurchaseIdChange(val) {
  emit('update:purchaseId', val)
}

// 订单详情变更
function handleOrderInfoChange(val) {
  emit('purchase-order-change', val)
}

// 解析导入表格
function handleExcelSuccess(val) {
  console.log(val)
}

// 跳转到入库记录
function toInboundRecord() {}

// 查看申购单
function openRequisitionsView() {}

// 表单校验
async function validate() {
  if (formRef.value) {
    const res = await formRef.value.validate()
    return res
  } else {
    return false
  }
}

// 外部调用
defineExpose({
  validate
})
</script>
<style lang="scss" scoped>
.inbound-application-header {
  margin-bottom: 8px;
  ::v-deep(.el-form-item) {
    margin-bottom: 0;
  }
}
</style>
