<template>
  <common-dialog
    custom-class="inbound-application-preview"
    :title="order.serialNumber ? `订单：${order.serialNumber}（${order.supplier ? order.supplier.name : ''}）` : '甲供入库'"
    append-to-body
    v-model="dialogVisible"
    width="1200px"
    :before-close="handleClose"
    :top="'5vh'"
    fullscreen
  >
    <template #titleAfter>
      <title-after-info v-if="!boolPartyA" :order="order" :detail="form" />
    </template>
    <template #titleRight>
      <purchase-detail-button v-if="fillableAmount" :purchase-id="order.id" size="mini" />
    </template>
    <!-- 不刷新组件无法正常更新 -->
    <template v-if="dialogVisible">
      <el-form ref="formRef" :model="form" :disabled="cu.status.edit === FORM.STATUS.PROCESSING">
        <common-table
          :data="form.list"
          :data-format="columnsDataFormat"
          :max-height="maxHeight"
          show-summary
          :cell-class-name="wrongCellMask"
          :summary-method="getSummaries"
          :expand-row-keys="expandRowKeys"
          row-key="uid"
        >
          <!-- 次要信息：当列过多的时候，在展开处显示次要信息 -->
          <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="uid" fixed="left">
            <template #default="{ row }">
              <expand-secondary-info v-if="!showTableColumnSecondary" :basic-class="row.basicClass" :row="row" show-brand />
              <p>
                备注：<span>{{ row.remark }}</span>
              </p>
            </template>
          </el-expand-table-column>
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="props.basicClass" fixed="left" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns :basic-class="props.basicClass" />
          <!-- 次要信息 -->
          <material-secondary-info-columns v-if="showTableColumnSecondary" :basic-class="props.basicClass" />

          <template v-if="fillableAmount && !boolPartyA">
            <el-table-column key="unitPrice" prop="unitPrice" align="right" width="120" label="含税单价">
              <template #default="{ row: { sourceRow: row } }">
                <span>{{ row.unitPrice }}</span>
              </template>
            </el-table-column>
            <el-table-column key="amount" prop="amount" align="right" width="120" label="金额">
              <template #default="{ row }">
                <span>{{ row.amount }}</span>
              </template>
            </el-table-column>
          </template>

          <!-- 金额设置 -->
          <!-- <price-set-columns
            v-if="fillableAmount"
            :form="form"
            :order="order"
            :show-price="false"
            :requisitions="cu.props.requisitions"
            @amount-change="handleAmountChange"
          /> -->
          <!-- 项目设置 -->
          <project-set-columns :form="form" :order="order" :requisitions="cu.props.requisitions" />
          <!-- 仓库设置 -->
          <warehouse-set-columns :form="form" v-if="fillableWarehouse" />
        </common-table>
        <!-- 物流信息设置 -->
        <logistics-form
          v-if="fillableLogistics"
          ref="logisticsRef"
          class="logistics-form-content"
          :disabled="cu.status.edit === FORM.STATUS.PROCESSING"
          :form="form.logistics"
        />
      </el-form>
    </template>
    <common-footer class="footer" :show-total="false" is-submit />
  </common-dialog>
</template>

<script setup>
import { computed, defineEmits, defineProps, provide, ref, watch } from 'vue'
import { orderSupplyTypeEnum, inboundFillWayEnum } from '@enum-ms/wms'
import { STEEL_ENUM } from '@/settings/config'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { logisticsPayerEnum } from '@/utils/enum/modules/logistics'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
// import { isBlank, isNotBlank, toFixed } from '@/utils/data-type'
import { isBlank, isNotBlank } from '@/utils/data-type'
import { materialColumns } from '@/utils/columns-format/wms'
import { DP } from '@/settings/config'

import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
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

import logisticsForm from '@/views/wms/material-inbound/raw-material/components/logistics-form.vue'
// import priceSetColumns from '@/views/wms/material-inbound/raw-material/components/price-set-columns.vue'
import projectSetColumns from '@/views/wms/material-inbound/raw-material/components/project-set-columns.vue'
import warehouseSetColumns from '@/views/wms/material-inbound/raw-material/components/warehouse-set-columns.vue'
import titleAfterInfo from '@/views/wms/material-inbound/raw-material/components/title-after-info.vue'
import commonFooter from './common-footer.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()
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

// 表格列数据格式转换
const columnsDataFormat = computed(() => {
  return [
    ...materialColumns,
    // 金额相关
    ['invoiceType', ['parse-enum', invoiceTypeEnum, { f: 'SL' }]],
    ['taxRate', ['suffix', '%']],
    ['unitPrice', ['to-thousand', decimalPrecision.value.wms]],
    ['unitPriceExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['amount', ['to-thousand', DP.YUAN]],
    ['amountExcludingVAT', ['to-thousand', DP.YUAN]],
    ['inputVAT', ['to-thousand', DP.YUAN]],
    ['brand', 'empty-text'],
    ['heatNoAndBatchNo', 'empty-text'],
    ['remark', 'empty-text']
  ]
})
// const columnsDataFormat = ref([
//   ...materialHasAmountColumns,
//   ['brand', 'empty-text'],
//   ['heatNoAndBatchNo', 'empty-text'],
//   ['remark', 'empty-text']
// ])

// 仓管填写的信息（工厂及仓库）
const warehouseRules = {
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
  warehouseId: [{ required: true, message: '请选择仓库', trigger: 'change' }]
}

// 采购填写的信息（金额、申购单及项目）
const amountRules = {
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  amount: [{ required: true, message: '请填写金额', trigger: 'blur' }]
}

// 项目
const projectRules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }]
}

const tableRules = computed(() => {
  const rules = { ...warehouseRules }
  // 甲供不填写金额方面的信息
  if (fillableAmount.value && !boolPartyA.value) {
    Object.assign(rules, amountRules)
    if (isNotBlank(order.value.projects)) {
      Object.assign(rules, projectRules)
    }
  }
  // 甲供项目必填
  if (boolPartyA.value) {
    Object.assign(rules, projectRules)
  }
  return rules
})

const expandRowKeys = ref([]) // 展开行key
// const amount = ref() // 金额

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, closeHook: closeHook })
const { cu, form, FORM } = regExtra() // 表单
const { inboundFillWayCfg } = useWmsConfig()

// 物流组件ref
const logisticsRef = ref()
// 订单信息
const order = computed(() => cu.props.order || {})
// 显示金额相关信息（由采购填写的信息）
// const fillableAmount = ref(true)
const fillableAmount = computed(() =>
  inboundFillWayCfg.value ? inboundFillWayCfg.value.amountFillWay === inboundFillWayEnum.APPLICATION.V : false
)
// 显示仓库（由仓库填写的信息）
const fillableWarehouse = ref(true)
// const fillableWarehouse = computed(() => inboundFillWayCfg.value ? inboundFillWayCfg.value.warehouseFillWay === inboundFillWayEnum.APPLICATION.V : false)
// 显示物流信息
const fillableLogistics = computed(() => order.value.logisticsPayerType === logisticsPayerEnum.DEMAND.V && fillableAmount.value)
// 是否“甲供”
const boolPartyA = computed(() => order.value?.supplyType === orderSupplyTypeEnum.PARTY_A.V)
// 在列中显示次要信息
const showTableColumnSecondary = computed(() => {
  // 非甲供订单，显示项目和申购单 或者仓库时
  const unshow1 =
    fillableAmount.value && !boolPartyA.value && ((order.value.projects && order.value.requisitionsSN) || fillableWarehouse.value)
  // 甲供订单，显示项目和申购单以及仓库时
  const unshow2 = fillableAmount.value && boolPartyA.value && order.value.projects && order.value.requisitionsSN && fillableWarehouse.value
  return !(unshow1 || unshow2)
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
  ['monomerId', -1],
  ['areaId', -1],
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
  if (form.supplyType & orderSupplyTypeEnum.PARTY_A.V) {
    form.purchaseId = undefined
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
  if (fillableLogistics.value && logisticsRef.value) {
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
// function handleAmountChange() {
//   if (!form.list) return
//   amount.value = toFixed(
//     form.list.reduce((sum, cur) => {
//       const value = Number(cur.amount)
//       if (!isNaN(value)) {
//         return sum + cur.amount
//       } else {
//         return sum
//       }
//     }, 0),
//     2
//   )
// }

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity', 'mete', ['amount', DP.YUAN]],
    toThousandFields: ['mete', 'amount']
  })
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
