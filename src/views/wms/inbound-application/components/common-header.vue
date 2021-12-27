<template>
  <div class="inbound-application-header flex-rbc">
    <div>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-position="right" inline label-width="80px">
        <el-form-item label="订单号" prop="purchaseId" label-width="70px">
          <purchase-sn-select
            class="input-underline"
            v-model="form.purchaseId"
            v-model:info="purchaseOrderInfo"
            :basic-class="props.basicClass"
            @change="handlePurchaseIdChange"
            @info-change="handleOrderInfoChange"
            style="width: 300px"
          />
        </el-form-item>
        <el-form-item label="车牌号" prop="licensePlate" label-width="70px">
          <el-input class="input-underline" v-model.trim="form.licensePlate" placeholder="请输入车牌号" style="width: 125px" />
        </el-form-item>
        <el-form-item
          v-if="props.basicClass & STEEL_ENUM && orderInfo.weightMeasurementMode === weightMeasurementModeEnum.OVERWEIGHT.V"
          :label="`过磅重量(千克)`"
          label-width="120px"
          prop="loadingWeight"
        >
          <el-tooltip
            class="item"
            effect="dark"
            :content="`重量误差：${trainsDiff.overNum} kg， ${overDiffTip}`"
            :disabled="!trainsDiff.hasOver"
            placement="top"
          >
            <common-input-number
              v-model="form.loadingWeight"
              class="input-underline"
              style="width: 135px"
              :min="0"
              :max="9999999"
              :controls="false"
              :precision="3"
              :placeholder="`输入该车次重量`"
              :class="{ 'over-weight-tip': trainsDiff.hasOver }"
            />
          </el-tooltip>
        </el-form-item>
      </el-form>
    </div>
    <div class="child-mr-7">
      <store-operation v-if="!props.edit" type="cu" @clear="handleClear" />
      <common-button type="primary" size="mini" @click="openRequisitionsView">查看申购单</common-button>
      <el-tooltip content="请先选择采购订单" :disabled="!!form.purchaseId" placement="bottom" effect="light">
        <excel-resolve-button
          icon="el-icon-upload2"
          btn-name="批量导入"
          btn-size="mini"
          btn-type="success"
          :disabled="!form.purchaseId"
          @success="handleExcelSuccess"
        />
      </el-tooltip>
      <common-button icon="el-icon-time" type="info" size="mini" @click="toInboundRecord" />
    </div>
  </div>
</template>

<script setup>
import { getRequisitionsDetailBySN } from '@/api/wms/requisitions'
import { defineProps, defineEmits, defineExpose, ref, computed, watchEffect, nextTick, inject } from 'vue'
import { useRouter } from 'vue-router'
import { STEEL_ENUM } from '@/settings/config'
import { weightMeasurementModeEnum } from '@enum-ms/finance'
import { patternLicensePlate } from '@/utils/validate/pattern'

import { regExtra } from '@/composables/form/use-form'
import useWeightOverDiff from '@/composables/wms/use-trains-weight-over-diff'
import excelResolveButton from '@/components-system/common/excel-resolve-button/index.vue'
import purchaseSnSelect from '@/components-system/wms/purchase-sn-select/index.vue'
import { isBlank } from '@/utils/data-type'
import StoreOperation from '@crud/STORE.operation.vue'

const emit = defineEmits(['update:purchaseId', 'purchase-order-change'])

const props = defineProps({
  purchaseId: {
    type: String
  },
  basicClass: {
    type: Number
  },
  edit: {
    type: Boolean,
    default: false
  }
})

const router = useRouter()

const matSpecRef = inject('matSpecRef') // 调用父组件matSpecRef
const { cu, form, FORM } = regExtra() // 表单
const { overDiffTip, weightOverDiff, diffSubmitValidate } = useWeightOverDiff() // 过磅重量超出理论重量处理

const validateLoadingWeight = (rule, value, callback) => {
  if (diffSubmitValidate(trainsDiff.value.hasOver)) {
    callback()
    return
  } else {
    callback(new Error('超出误差允许范围'))
    return
  }
}

const rules = {
  purchaseId: [{ required: true, message: '请选择订单', trigger: 'change' }],
  licensePlate: [
    { required: true, message: '请填写车牌号', trigger: 'blur' },
    { pattern: patternLicensePlate, message: '请填写正确的车牌号', trigger: 'blur' }
  ],
  loadingWeight: [
    { required: true, message: '请填写过磅重量', trigger: 'blur' },
    { validator: validateLoadingWeight, trigger: 'blur' }
  ]
}

const formRef = ref()
// const form = ref(deepClone(defaultForm))
const purchaseOrderInfo = ref({})
const trainsDiff = ref({})

const orderInfo = computed(() => {
  return purchaseOrderInfo.value || {}
})

watchEffect(() => {
  trainsDiff.value = weightOverDiff(form.loadingWeight, cu.props.totalWeight)
})

// 提交后清除校验结果
FORM.HOOK.afterSubmit = () => {
  formRef.value.resetFields()
  init()
}

// 重置表单
function handleClear() {
  nextTick(() => {
    formRef.value.clearValidate()
    init()
  })
}

function init() {
  trainsDiff.value = {}
  // 清除选中
  const trigger = watchEffect(() => {
    if (matSpecRef.value) {
      matSpecRef.value.clear()
      nextTick(() => {
        trigger()
      })
    }
  })
}

// 采购订单id变更
function handlePurchaseIdChange(val) {
  emit('update:purchaseId', val)
}

// 订单详情变更
function handleOrderInfoChange(val) {
  cu.props.requisitions = {} // 初始化申购单
  // 获取申购单详情
  if (val && val.requisitionsSN) {
    fetchRequisitionsDetail(val.requisitionsSN)
  }
  emit('purchase-order-change', val)
}

// 加载申购单
async function fetchRequisitionsDetail(snArr) {
  if (isBlank(snArr)) return
  const allInterFace = []
  snArr.forEach((sn) => {
    const promiseItem = getRequisitionsDetailBySN(sn).then((detail) => {
      cu.props.requisitions[sn] = detail
    })
    allInterFace.push(promiseItem)
  })
  await Promise.all(allInterFace)
}

// 解析导入表格
function handleExcelSuccess(val) {
  console.log(val)
}

// TODO:跳转到入库记录
function toInboundRecord() {
  router.push({ name: 'RawMatInboundApplicationRecord', params: { basicClass: props.basicClass }})
}

// 查看申购单
function openRequisitionsView() {}

// 表单校验
async function validate() {
  try {
    if (formRef.value) {
      const res = await formRef.value.validate()
      return res
    } else {
      return false
    }
  } catch (error) {
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
