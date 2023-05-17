<template>
  <div class="inbound-application-header flex-rbc">
    <div>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-position="right" inline label-width="80px">
        <el-form-item v-if="!props.isManuf" prop="supplyType" label-width="0px">
          <common-radio-button
            v-model="form.supplyType"
            :options="orderSupplyTypeEnum.ENUM"
            default
            type="enumSL"
            :style="!edit ? 'margin-left:10px' : ''"
            style="vertical-align: middle"
          >
            <template #suffix>
              <span>入库</span>
            </template>
          </common-radio-button>
        </el-form-item>
        <el-form-item
          v-if="!boolPartyA"
          prop="purchaseId"
          :label="props.isManuf ? '订单号' : ''"
          :label-width="props.isManuf ? '70px' : '0px'"
        >
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
            v-if="props.basicClass & STEEL_ENUM && orderInfo.weightMeasurementMode === weightMeasurementModeEnum.OVERWEIGHT.V"
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
      <!-- <common-button type="primary" size="mini" @click="openRequisitionsView">查看申购单</common-button> -->
      <el-tooltip
        content="请先选择采购合同编号"
        :disabled="!!form.purchaseId && form.supplyType === orderSupplyTypeEnum.SELF.V"
        placement="bottom"
        effect="light"
        v-if="boolPartyA"
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
import { inboundDetail as getPurchaseOrderDetail } from '@/api/supply-chain/purchase-order'
import { defineProps, defineEmits, defineExpose, ref, computed, watch, watchEffect, nextTick, inject } from 'vue'
import { useRouter } from 'vue-router'
import { STEEL_ENUM } from '@/settings/config'
import { orderSupplyTypeEnum } from '@/utils/enum/modules/wms'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { weightMeasurementModeEnum } from '@enum-ms/finance'
import { logisticsPayerEnum, logisticsTransportTypeEnum } from '@/utils/enum/modules/logistics'
import { patternLicensePlate } from '@/utils/validate/pattern'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

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
  isManuf: {
    type: Boolean,
    default: false
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

const boolPartyA = computed(() => form.supplyType === orderSupplyTypeEnum.PARTY_A.V)

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
    if (form.supplyType && form.supplyType === orderSupplyTypeEnum.PARTY_A.V) {
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
  },
  { immediate: true }
)

// 采购合同编号id变更
function handlePurchaseIdChange(val) {
  nextTick(() => {
    trainsDiff.value = {}
    formRef?.value?.clearValidate()
  })
  form.purchaseId = val
  emit('update:purchaseId', val)
}

// 订单详情变更
async function handleOrderInfoChange(order, oldOrder) {
  cu.props.requisitions = {} // 初始化申购单
  if (!form.selectObj) form.selectObj = {}
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
    if (order.applyPurchase?.length) {
      order.boolApplyPurchase = true
      order.projects = []
    } else {
      order.boolApplyPurchase = false
    }
    if (order.id) {
      const { content = [] } = await getPurchaseOrderDetail(order.id)
      await setSpecInfoToList(content)
      await numFmtByBasicClass(
        content,
        {
          toNum: true
        },
        { mete: ['mete', 'inboundMete'] }
      )
      order.details = content?.map((v) => {
        const applyPurchaseObj = {}
        v.mergeId = v.id // 处理合并问题
        v.purchaseOrderDetailId = v.id
        v.purchaseQuantity = v.quantity
        v.purchaseMete = v.mete
        v.boolApplyPurchase = false
        v.canPurchaseQuantity = v.purchaseQuantity - v.inboundQuantity > 0 ? v.purchaseQuantity - v.inboundQuantity : 0
        // const _canMete = toPrecision(v.purchaseMete - v.inboundMete)
        let _isSelected = false
        if (form.selectObj?.[v.mergeId]) {
          _isSelected = form.selectObj[v.mergeId]?.isSelected
        } else if (props.edit && form?.editObj?.[v.mergeId]) {
          _isSelected = form?.editObj?.[v.mergeId]?.isSelected
        }
        // v.quantity = v.canPurchaseQuantity
        // v.mete = _canMete > 0 ? _canMete : 0
        let _v = v
        if (_isSelected) {
          _v = {
            ..._v,
            ...form?.selectObj?.[v.mergeId],
            ...form?.editObj?.[v.mergeId]
          }
        }
        form.selectObj[v.mergeId] = {
          ..._v,
          isSelected: _isSelected
        }
        if (_v.applyPurchase?.length) {
          _v.boolApplyPurchase = true
          let _totalMete = 0
          let _totalQuantity = 0
          _v.applyPurchase.forEach((item) => {
            item.applyPurchaseSN = item.serialNumber
            applyPurchaseObj[item.id] = item
            if (_v.basicClass & (matClsEnum.SECTION_STEEL.V | matClsEnum.STEEL_PLATE.V)) {
              const _originInfo = !props.edit ? _v : form?.editObj?.[_v.mergeId]
              item.sn = _originInfo?.sn
              item.specificationLabels = _originInfo?.specificationLabels // 规格中文
              item.serialNumber = _originInfo?.serialNumber // 科目编号 - 规格
              item.classifyId = _originInfo?.classifyId // 科目id
              item.classifyFullName = _originInfo?.classifyFullName // 全路径名称
              item.classifyName = _originInfo?.classifyName // 当前科目名称
              item.classifyParentFullName = _originInfo?.classifyParentFullName // 父级路径名称
              item.basicClass = _originInfo?.basicClass // 基础类型
              item.specification = _originInfo?.specification // 规格
              item.specificationMap = _originInfo?.specificationMap // 规格KV格式
              item.measureUnit = _originInfo?.measureUnit // 计量单位
              item.accountingUnit = _originInfo?.accountingUnit // 核算单位
              item.measurePrecision = _originInfo?.measurePrecision // 计量精度
              item.accountingPrecision = _originInfo?.accountingPrecision // 核算精度
              item.unitWeight = _originInfo?.unitWeight // 单位重量
              item.length = _originInfo?.length
              item.width = _originInfo?.width
              item.thickness = _originInfo?.thickness
            }
            if (!_isSelected || props.edit) {
              item.applyPurchaseId = item.id
              item.purchaseQuantity = item.quantity
              item.purchaseMete = item.mete
              item.quantity = null
              item.mete = null
            }
            if (props.edit && form?.editObj?.[_v.mergeId]?.applyPurchaseObj?.[item.applyPurchaseId]) {
              const _quantity = form?.editObj?.[_v.mergeId]?.applyPurchaseObj?.[item.applyPurchaseId]?.quantity
              const _mete = form?.editObj?.[_v.mergeId]?.applyPurchaseObj?.[item.applyPurchaseId]?.mete
              item.quantity = _quantity
              _totalQuantity += _quantity
              item.mete = _mete
              _totalMete += _mete
            } else {
              _totalQuantity += item.quantity || 0
              _totalMete += item.mete || 0
            }
            item.originQuantity = item.quantity
            item.originMete = item.mete
            if (item?.project) {
              order.projects.push(item?.project)
            }
          })
          _v.quantity = _totalQuantity
          _v.mete = _totalMete
        } else {
          _v.quantity = null
          _v.mete = null
        }
        if (props.edit && form?.editObj?.[_v.mergeId]) {
          _v.quantity = form?.editObj?.[_v.mergeId]?.quantity
          _v.mete = form?.editObj?.[_v.mergeId]?.mete
        } else if (_isSelected) {
          _v.quantity = form?.selectObj?.[_v.mergeId]?.quantity
          _v.mete = form?.selectObj?.[_v.mergeId]?.mete
        }
        _v.originQuantity = _v.quantity
        _v.originMete = _v.mete
        if (props.edit) {
          _v.needFirstCalcTheoryWeight = true
        }

        if (v.inboundList?.length) {
          setSpecInfoToList(v.inboundList)
          numFmtByBasicClass(
            v.inboundList,
            {
              toNum: true
            }
          )
          v.inboundList.forEach((item) => {
            item.applyPurchaseSN = applyPurchaseObj[item.applyPurchaseId]?.applyPurchaseSN
          })
        }
        return _v
      })
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
// function openRequisitionsView() {}

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
