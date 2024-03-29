<template>
  <div class="inbound-application-header flex-rbc">
    <div>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-position="right" inline label-width="80px">
        <el-form-item prop="supplyType" label-width="0px">
          <common-radio-button
            v-model="form.supplyType"
            :options="orderSupplyTypeEnum.ENUM"
            default
            type="enumSL"
            :style="!edit?'margin-left:10px':''"
            style="vertical-align: middle"
          >
            <template #suffix>
              <span>入库</span>
            </template>
          </common-radio-button>
        </el-form-item>
        <el-form-item v-if="form.supplyType === orderSupplyTypeEnum.SELF.V" prop="purchaseId" label-width="0px">
          <purchase-sn-select
            class="input-underline"
            v-model="form.purchaseId"
            :basic-class="props.basicClass"
            @change="handlePurchaseIdChange"
            @info-change="handleOrderInfoChange"
            style="width: 300px"
          />
        </el-form-item>
        <template v-if="isNotBlank(orderInfo)">
          <el-form-item
            v-if="orderInfo.logisticsTransportType === logisticsTransportTypeEnum.FREIGHT.V"
            label="车牌号"
            prop="licensePlate"
            label-width="70px"
          >
            <el-input class="input-underline" v-model.trim="form.licensePlate" placeholder="车牌号" style="width: 125px" />
          </el-form-item>
          <el-form-item
            v-if="orderInfo.logisticsTransportType === logisticsTransportTypeEnum.POST.V"
            label="物流单号"
            prop="shipmentNumber"
            label-width="70px"
          >
            <el-input
              class="input-underline"
              v-model.trim="form.shipmentNumber"
              placeholder="物流单号"
              maxlength="100"
              style="width: 200px"
            />
          </el-form-item>
          <el-form-item
            v-if="props.basicClass & STEEL_ENUM && orderInfo.weightMeasurementMode !== weightMeasurementModeEnum.THEORY.V"
            :label="`车次过磅重量(kg)`"
            label-width="150px"
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
                mode="input"
                :min="0"
                :max="999999999"
                :controls="false"
                :precision="0"
                :placeholder="`输入该车次重量`"
                :class="{ 'over-weight-tip': trainsDiff.hasOver }"
              />
            </el-tooltip>
          </el-form-item>
        </template>
      </el-form>
    </div>
    <div class="child-mr-7">
      <store-operation v-if="!props.edit" type="cu" @clear="handleClear" />
      <common-button type="primary" size="mini" @click="openRequisitionsView">查看申购单</common-button>
      <el-tooltip
        content="请先选择采购合同编号"
        :disabled="!!form.purchaseId && form.supplyType === orderSupplyTypeEnum.SELF.V"
        placement="bottom"
        effect="light"
      >
        <excel-resolve-button
          icon="el-icon-upload2"
          btn-name="批量导入"
          btn-size="mini"
          btn-type="success"
          open-loading
          :template="importTemp"
          :disabled="!form.purchaseId && form.supplyType === orderSupplyTypeEnum.SELF.V"
          @success="handleExcelSuccess"
        />
      </el-tooltip>
      <common-button v-if="!props.edit" icon="el-icon-time" type="info" size="mini" @click="toInboundRecord" />
    </div>
  </div>
</template>

<script setup>
import { getRequisitionsDetailBySN } from '@/api/wms/requisitions'
import { defineProps, defineEmits, defineExpose, ref, computed, watch, watchEffect, nextTick, inject } from 'vue'
import { useRouter } from 'vue-router'
import { STEEL_ENUM } from '@/settings/config'
import { orderSupplyTypeEnum } from '@/utils/enum/modules/wms'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { weightMeasurementModeEnum } from '@enum-ms/finance'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { patternLicensePlate } from '@/utils/validate/pattern'

import useUserProjects from '@compos/store/use-user-projects'
import { regExtra } from '@/composables/form/use-form'
import useWeightOverDiff from '@/composables/wms/use-trains-weight-over-diff'
import excelResolveButton from '@/components-system/common/excel-resolve-button/index.vue'
import purchaseSnSelect from '@/components-system/wms/purchase-sn-select/index.vue'
import { isNotBlank, isBlank } from '@/utils/data-type'
import StoreOperation from '@crud/STORE.operation.vue'
import steelPlateTemp from '@/utils/excel/import-template/wms/inbound-application-temp/steel-plate'
import sectionSteelTemp from '@/utils/excel/import-template/wms/inbound-application-temp/section-steel'
import steelCoilTemp from '@/utils/excel/import-template/wms/inbound-application-temp/steel-coil'
import auxMaterialTemp from '@/utils/excel/import-template/wms/inbound-application-temp/aux-material'
import otherMaterialTemp from '@/utils/excel/import-template/wms/inbound-application-temp/other'
import gasTemp from '@/utils/excel/import-template/wms/inbound-application-temp/gas'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['update:purchaseId', 'purchase-order-change'])

const props = defineProps({
  purchaseId: {
    type: String
  },
  basicClass: {
    type: Number
  },
  validate: {
    type: Function
  },
  currentBasicClass: {
    type: Number
  },
  edit: {
    type: Boolean,
    default: false
  }
})

const router = useRouter()

const { projects } = useUserProjects()

const matSpecRef = inject('matSpecRef') // 调用父组件matSpecRef
const { cu, form, FORM } = regExtra() // 表单
const { overDiffTip, weightOverDiff, diffSubmitValidate } = useWeightOverDiff() // 过磅重量超出理论重量处理

const validateLoadingWeight = (rule, value, callback) => {
  // 为空通过
  if (value === undefined || value === null) {
    callback()
    return
  }
  // 范围内通过
  if (diffSubmitValidate(trainsDiff.value.hasOver)) {
    callback()
    return
  } else {
    callback(new Error('超出误差允许范围'))
    return
  }
}

// 基础校验规则
const baseRules = {
  purchaseId: [{ required: true, message: '请选择订单', trigger: 'change' }],
  licensePlate: [{ pattern: patternLicensePlate, message: '请填写正确的车牌号', trigger: 'blur' }],
  loadingWeight: [{ validator: validateLoadingWeight, trigger: 'blur' }]
}

// 磅计校验规则
const overWeightRules = {
  loadingWeight: [
    { required: true, message: '请填写过磅重量', trigger: 'blur' },
    { validator: validateLoadingWeight, trigger: 'blur' }
  ]
}

// 自提车牌校验规则
const licensePlateRules = {
  licensePlate: [
    { required: true, message: '请填写车牌号', trigger: 'blur' },
    { pattern: patternLicensePlate, message: '请填写正确的车牌号', trigger: 'blur' }
  ]
}

const rules = computed(() => {
  const rules = Object.assign({}, baseRules)
  if (orderInfo.value.logisticsTransportType === logisticsTransportTypeEnum.FREIGHT.V) {
    // 自提填写车牌号
    if (orderInfo.value.logisticsPayerType === logisticsPayerEnum.DEMAND.V) {
      Object.assign(rules, licensePlateRules)
    }
  }
  // 磅计过磅重量必填，混合计选填，理计不填（TODO:待定）
  if (props.basicClass & STEEL_ENUM && orderInfo.value.weightMeasurementMode === weightMeasurementModeEnum.OVERWEIGHT.V) {
    Object.assign(rules, overWeightRules)
  }
  return rules
})

// 当前物料“批量导入模板”
const importTemp = computed(() => {
  switch (props.currentBasicClass ?? props.basicClass) {
    case matClsEnum.STEEL_PLATE.V:
      return steelPlateTemp
    case matClsEnum.SECTION_STEEL.V:
      return sectionSteelTemp
    case matClsEnum.STEEL_COIL.V:
      return steelCoilTemp
    case matClsEnum.MATERIAL.V:
      return auxMaterialTemp
    case matClsEnum.OTHER.V:
      return otherMaterialTemp
    case matClsEnum.GAS.V:
      return gasTemp
    default:
      return auxMaterialTemp
  }
})

const formRef = ref()
const trainsDiff = ref({})
const orderInfo = ref({})

watchEffect(() => {
  trainsDiff.value = weightOverDiff(form.loadingWeight, cu.props.totalWeight)
  // 在入库列表重量发生变化时，触发校验
  formRef.value && formRef.value.validateField('loadingWeight')
})

// 提交后清除校验结果
FORM.HOOK.afterSubmit = () => {
  init()
  nextTick(() => {
    formRef.value && formRef.value.resetFields()
  })
}

// 重置表单
function handleClear() {
  init()
  nextTick(() => {
    formRef.value && formRef.value.clearValidate()
  })
}

function init() {
  trainsDiff.value = {}
  orderInfo.value = {}
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

watch(
  () => form.supplyType,
  () => {
    if (form.supplyType) {
      if (form.supplyType === orderSupplyTypeEnum.PARTY_A.V) {
        handlePurchaseIdChange(undefined)
        const _order = {
          supplyType: form.supplyType,
          weightMeasurementMode: weightMeasurementModeEnum.THEORY.V,
          basicClass: props.basicClass,
          projects: projects.value
        }
        handleOrderInfoChange(_order)
      } else {
        handlePurchaseIdChange(undefined)
        handleOrderInfoChange(undefined)
      }
    }
  },
  { immediate: true }
)

// 采购合同编号id变更
function handlePurchaseIdChange(val) {
  nextTick(() => {
    trainsDiff.value = {}
    formRef.value.clearValidate()
  })
  emit('update:purchaseId', val)
}

// 订单详情变更
function handleOrderInfoChange(order, oldOrder) {
  cu.props.requisitions = {} // 初始化申购单
  if (order) {
    // 获取申购单详情
    if (order.requisitionsSN) {
      fetchRequisitionsDetail(order.requisitionsSN)
    }
    // 物流运输方式更换后，清空对应信息
    if (order.logisticsTransportType === logisticsTransportTypeEnum.POST.V) {
      form.licensePlate = undefined
    }
    if (order.logisticsTransportType === logisticsTransportTypeEnum.FREIGHT.V) {
      form.shipmentNumber = undefined
    }
    // 当订单切换时，若订单计量方式发生变化，则重置车次过磅重量
    if (orderInfo.value && orderInfo.value.weightMeasurementMode !== order.weightMeasurementMode) {
      form.loadingWeight = undefined
    }
  }
  // 订单信息对象重新赋值
  orderInfo.value = order || {}
  emit('purchase-order-change', order, oldOrder)
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
function handleExcelSuccess(list) {
  // 解析
  // 根据物料种类获取
  try {
    cu.props.import(list)
    props.validate()
  } catch (error) {
    ElMessage.error({ message: error.message, duration: 5000 })
  }
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
