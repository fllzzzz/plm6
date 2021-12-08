<template>
  <common-dialog
    custom-class="inbound-application-preview"
    :title="`订单：${order.serialNumber}（${order.supplier ? order.supplier.name : ''}）`"
    append-to-body
    v-model="dialogVisible"
    width="1200px"
    :before-close="handleClose"
    :top="'5vh'"
    fullscreen
  >
    <template #titleAfter>
      <el-tag effect="plain">{{ `车牌：${form.licensePlate}` }}</el-tag>
      <el-tag v-if="props.basicClass & STEEL_ENUM && order.weightMeasurementMode !== weightMeasurementModeEnum.THEORY.V" effect="plain">
        {{ `过磅重量：${form.loadingWeight}kg` }}
      </el-tag>
      <el-tag v-parse-enum="{ e: orderSupplyTypeEnum, v: order.supplyType }" type="info" effect="plain" />
      <el-tag v-parse-enum="{ e: weightMeasurementModeEnum, v: order.weightMeasurementMode }" type="info" effect="plain" />
      <el-tag v-parse-enum="{ e: purchaseOrderPaymentModeEnum, v: order.purchaseOrderPaymentMode }" type="info" effect="plain" />
      <el-tag v-parse-enum="{ e: pickUpModeEnum, v: order.pickUpMode }" type="info" effect="plain" />
    </template>
    <template #titleRight>
      <purchase-detail-button v-if="showAmount" :purchase-id="order.id" size="mini" />
    </template>
    <!-- 不刷新组件无法正常更新 -->
    <template v-if="dialogVisible">
      <el-form ref="formRef" :model="form" :disabled="cu.status.edit === FORM.STATUS.PROCESSING">
        <common-table
          :data="form.list"
          :max-height="maxHeight"
          show-summary
          :cell-class-name="wrongCellMask"
          :summary-method="getSummaries"
          :expand-row-keys="expandRowKeys"
          row-key="uid"
        >
          <!-- 次要信息：当列过多的时候，在展开处显示次要信息-->
          <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="uid" fixed="left">
            <template #default="{ row }">
              <expand-secondary-info v-if="showAmount || showWarehouse" :basic-class="props.basicClass" :row="row" show-brand />
              <p>
                备注：<span v-empty-text>{{ row.remark }}</span>
              </p>
            </template>
          </el-expand-table-column>
          <el-table-column label="序号" type="index" align="center" width="50" fixed="left" />
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="props.basicClass" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns :basic-class="props.basicClass" />
          <!-- 次要信息 -->
          <material-secondary-info-columns v-if="!(showAmount || showWarehouse)" :basic-class="props.basicClass" />
          <!-- 金额设置 -->
          <price-set-columns
            v-if="showAmount"
            :order="order"
            :form="form"
            :requisitions="cu.props.requisitions"
            @amount-change="handleAmountChange"
          />
          <!-- 仓库设置 -->
          <warehouse-set-columns :form="form" v-if="showWarehouse" />
        </common-table>
        <!-- 物流信息设置 -->
        <logistics-form
          v-if="showAmount && form.logistics"
          ref="logisticsRef"
          class="logistics-form-content"
          :disabled="cu.status.edit === FORM.STATUS.PROCESSING"
          :form="form.logistics"
        />
      </el-form>
    </template>
    <common-footer class="footer" unit="元" :total-value="amount" :show-total="showAmount" is-submit />
  </common-dialog>
</template>

<script setup>
import { computed, defineEmits, defineProps, provide, ref, watch } from 'vue'
import { inboundFillWayEnum, orderSupplyTypeEnum, pickUpModeEnum, purchaseOrderPaymentModeEnum } from '@enum-ms/wms'
import { weightMeasurementModeEnum } from '@enum-ms/finance'
import { STEEL_ENUM } from '@/settings/config'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

import { regExtra } from '@/composables/form/use-form'
import useTableValidate from '@/composables/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useWmsConfig from '@/composables/store/use-wms-config'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import purchaseDetailButton from '@/components-system/wms/purchase-detail-button/index.vue'

import logisticsForm from '@/views/wms/inbound-components/logistics-form.vue'
import priceSetColumns from '@/views/wms/inbound-components/price-set-columns.vue'
import warehouseSetColumns from '@/views/wms/inbound-components/warehouse-set-columns.vue'
import commonFooter from './common-footer.vue'
import { isBlank, isNotBlank, toFixed } from '@/utils/data-type'
import { matClsEnum } from '@/utils/enum/modules/classification'
// TODO:处理申购单与项目之间的关联
// TODO: 标签打印提示

const emit = defineEmits(['saveSuccess', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  }
})

const logisticsRef = ref()

// 显示金额
const showAmount = computed(() => inboundFillWayCfg.value.amountFillWay === inboundFillWayEnum.APPLICATION.V)
// 显示仓库
const showWarehouse = computed(() => inboundFillWayCfg.value.warehouseFillWay === inboundFillWayEnum.APPLICATION.V)

// 表格校验
const warehouseRules = {
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
  warehouseId: [{ required: true, message: '请选择仓库', trigger: 'change' }]
}

const amountRules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  amount: [{ required: true, message: '请填写金额', trigger: 'blur' }]
}

const tableRules = computed(() => {
  const rules = {}
  if (showAmount.value) Object.assign(rules, amountRules)
  if (showWarehouse.value) Object.assign(rules, warehouseRules)
  return rules
})

const expandRowKeys = ref([]) // 展开行key
const amount = ref() // 金额

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, closeHook: closeHook })
const { cu, form, FORM } = regExtra() // 表单
const { inboundFillWayCfg } = useWmsConfig()

// 订单信息
const order = computed(() => {
  return cu.props.order || {}
})

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.inbound-application-preview',
    extraBox: ['.el-dialog__header', '.logistics-form-content', '.footer'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  dialogVisible
)

// 同上的选项与值
const ditto = new Map([
  ['requisitionsSN', -1],
  ['projectId', -1],
  ['factoryId', -1],
  ['warehouseId', -1]
])
provide('ditto', ditto)
// 表格校验
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

watch(
  () => props.modelValue,
  (visible) => {
    if (visible) {
      setDitto(form.list) // 在list变化时设置同上
      form.list.forEach((v) => {
        if (isNotBlank(v.amount) && isNotBlank(v.mete)) {
          // 量发生变化，清空金额
          const unitPrice = toFixed(v.amount / v.mete, 2, { toNum: true })
          if (v.unitPrice !== unitPrice) {
            v.unitPrice = undefined
            v.amount = undefined
          }
        }
      })
    }
  },
  { immediate: true }
)

// 表单提交数据清理
cu.submitFormFormat = async (form) => {
  cleanUpData(form.list)
  form.list = await numFmtByBasicClass(form.list, { toSmallest: true, toNum: true })
  if (props.basicClass <= STEEL_ENUM) {
    // 钢材拆分为三个数组传递给服务端
    form.steelPlateList = []
    form.sectionSteelList = []
    form.steelCoilList = []
    form.list.forEach((v) => {
      switch (v.basicClass) {
        case matClsEnum.STEEL_PLATE.V:
          form.steelPlateList.push(v)
          break
        case matClsEnum.SECTION_STEEL.V:
          form.sectionSteelList.push(v)
          break
        case matClsEnum.STEEL_COIL.V:
          form.steelCoilList.push(v)
          break
      }
    })
  }
  return form
}

// 表单提交前校验
FORM.HOOK.beforeSubmit = async () => {
  const { validResult, dealList } = tableValidate(form.list)
  if (validResult) {
    form.list = dealList
  }
  let logisticsValidResult = true
  if (showAmount.value && logisticsRef.value) {
    logisticsValidResult = await logisticsRef.value.validate()
  }
  return validResult && logisticsValidResult
}

// 表单提交后：关闭预览窗口
FORM.HOOK.afterSubmit = () => {
  handleClose()
}

function closeHook() {
  // 关闭窗口时，取消所有选中，避免再次打开，数据无法及时更新
  expandRowKeys.value = []
}

// 设置同上
function setDitto(list) {
  if (isBlank(list)) return
  const dittoWithNotWare = new Map([
    ['requisitionsSN', -1],
    ['projectId', -1],
    ['factoryId', -1]
  ])
  let basicClass = list[0].basicClass // 首个不一样的物料类型，仓库位置不设置同上
  const warehouseDittoableIdex = [0]
  for (let i = 1; i < list.length; i++) {
    const row = list[i]
    if (basicClass === row.basicClass) {
      ditto.forEach((value, key) => {
        if (isBlank(row[key])) {
          row[key] = value
        }
      })
    } else {
      dittoWithNotWare.forEach((value, key) => {
        if (isBlank(row[key])) {
          row[key] = value
        }
      })
      basicClass = row.basicClass
      warehouseDittoableIdex.push(i)
    }
  }
}

// 金额变化
function handleAmountChange() {
  if (!form.list) return
  amount.value = toFixed(
    form.list.reduce((sum, cur) => {
      const value = Number(cur.amount)
      if (!isNaN(value)) {
        return sum + cur.amount
      } else {
        return sum
      }
    }, 0),
    2
  )
}

// 合计
function getSummaries(param) {
  return tableSummary(param, { props: ['quantity', 'mete'] })
}
</script>

<style lang="scss" scoped>
.inbound-application-preview {
  position: relative;
  .el-dialog__header .el-tag {
    min-width: 70px;
  }

  .logistics-form-content {
    padding: 0 30px;
    position: absolute;
    bottom: 50px;
    left: 0;
  }
  .footer {
    position: absolute;
    bottom: 0;
    left: 0;
  }
}
</style>
