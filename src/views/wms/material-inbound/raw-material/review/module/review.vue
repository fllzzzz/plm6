<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    :content-loading="detailLoading"
    :before-close="handleClose"
    :title="drawerTitle"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-inbound-application-review-form"
  >
    <template #titleAfter>
      <title-after-info :order="order" :detail="form" />
    </template>
    <template #titleRight>
      <review-convenient-operate
        ref="reviewConvenientRef"
        v-if="pendingReviewIdList && pendingReviewIdList.length > 1"
        v-model="currentInboundId"
        v-model:consequent="reviewNext"
        :list="pendingReviewIdList"
        @change="handleConvenientChange"
        class="convenient-operation"
      />
      <purchase-detail-button :purchase-id="order.id" size="mini" />
      <!-- 审核按钮 -->
      <review-confirm-button
        v-model:passed-loading="passedLoading"
        v-model:returned-loading="returnedLoading"
        :passed-fn="passed"
        :returned-fn="returned"
      />
    </template>
    <template #content>
      <el-form class="form" :disabled="formDisabled">
        <inspection-return-info
          class="inspection-return-info"
          v-if="form.returnList?.length"
          :basic-class="form.basicClass"
          :list="form.returnList"
          :showTableColumnSecondary="showTableColumnSecondary"
          :showAmount="showAmount"
          :boolPartyA="boolPartyA"
        />
        <common-table
          :data="form.list"
          :max-height="maxHeight"
          :dataFormat="columnsDataFormat"
          show-summary
          :cell-class-name="wrongCellMask"
          :summary-method="getSummaries"
          :expand-row-keys="expandRowKeys"
          row-key="id"
        >
          <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
            <template #default="{ row }">
              <expand-secondary-info v-if="!showTableColumnSecondary" :basic-class="row.basicClass" :row="row" show-brand />
              <p>
                单体：<span>{{ row.monomerName }}</span>
              </p>
              <p>
                区域：<span>{{ row.areaName }}</span>
              </p>
              <p>
                备注：<span>{{ row.remark }}</span>
              </p>
            </template>
          </el-expand-table-column>
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="form.basicClass" fixed="left" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns :basic-class="form.basicClass" />
          <!-- 次要信息 -->
          <material-secondary-info-columns v-if="showTableColumnSecondary" :basic-class="form.basicClass" />
          <!-- 金额设置 -->
          <template v-if="showAmount">
            <!-- <price-set-columns
              v-if="fillableAmount"
              :form="form"
              :order="order"
              :requisitions="requisitions"
              weightAttribute="mete"
              @amount-change="handleAmountChange"
            /> -->
            <template v-if="fillableAmount && !boolPartyA">
              <el-table-column prop="unitPrice" align="center" width="135px" label="含税单价">
                <template #default="{ row: { sourceRow: row } }">
                  <common-input-number
                    v-if="row"
                    v-model="row.unitPrice"
                    :min="0"
                    :max="9999999999"
                    :controls="false"
                    :step="1"
                    size="mini"
                    placeholder="含税单价"
                    @change="handleUnitPriceChange($event, row)"
                  />
                </template>
              </el-table-column>
              <el-table-column prop="amount" align="center" width="135px" label="金额">
                <template #default="{ row: { sourceRow: row } }">
                  <common-input-number
                    v-if="row"
                    v-model="row.amount"
                    :min="0"
                    :max="9999999999"
                    :controls="false"
                    :step="1"
                    size="mini"
                    :precision="DP.YUAN"
                    placeholder="金额"
                    @change="handleAmountChange($event, row)"
                  />
                </template>
              </el-table-column>
              <el-table-column prop="project" label="项目" align="left" min-width="120px" show-overflow-tooltip />
            </template>
            <template v-else>
              <el-table-column prop="unitPrice" label="含税单价" align="right" min-width="120px" show-overflow-tooltip />
              <el-table-column prop="amount" label="金额" align="right" min-width="120px" show-overflow-tooltip />
              <el-table-column prop="sourceRequisitionsSN" label="申购单" align="left" min-width="120px" show-overflow-tooltip />
              <el-table-column prop="project" label="项目" align="left" min-width="120px" show-overflow-tooltip />
              <el-table-column prop="monomerName" label="单体" align="left" min-width="120px" show-overflow-tooltip />
              <el-table-column prop="areaName" label="区域" align="left" min-width="120px" show-overflow-tooltip />
            </template>
          </template>
          <template v-else>
            <el-table-column prop="sourceRequisitionsSN" label="申购单" align="left" min-width="120px" show-overflow-tooltip />
            <el-table-column prop="project" label="项目" align="left" min-width="120px" show-overflow-tooltip />
            <el-table-column prop="monomerName" label="单体" align="left" min-width="120px" show-overflow-tooltip />
            <el-table-column prop="areaName" label="区域" align="left" min-width="120px" show-overflow-tooltip />
          </template>
          <!-- 仓库设置 -->
          <warehouse-set-columns v-if="fillableWarehouse" :form="form" />
          <warehouse-info-columns v-else />
        </common-table>
        <el-input
          class="approval-comments"
          v-model="form.approvalComments"
          :rows="2"
          maxlength="1000"
          type="textarea"
          show-word-limit
          placeholder="审核意见"
          style="margin-top: 5px"
        />
        <!-- 物流信息设置 -->
        <logistics-form
          v-if="fillableLogistics"
          ref="logisticsRef"
          class="logistics-form-content"
          :disabled="formDisabled"
          :form="form.logistics"
        />
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { getPendingReviewIdList, detail, reviewPassed, reviewReturned } from '@/api/wms/material-inbound/raw-material/review'
import { inject, computed, ref, defineEmits, defineProps, watch } from 'vue'
import { orderSupplyTypeEnum, inspectionStatusEnum, inboundFillWayEnum } from '@enum-ms/wms'
import { logisticsPayerEnum } from '@/utils/enum/modules/logistics'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
// import { deepClone, isBlank, isNotBlank, toFixed } from '@/utils/data-type'
import { deepClone, isBlank, toPrecision, isNotBlank } from '@/utils/data-type'
import { getDP } from '@/utils/data-type/number'
import { DP } from '@/settings/config'
import { materialHasAmountColumns } from '@/utils/columns-format/wms'

import { regExtra } from '@compos/use-crud'
import useTableValidate from '@/composables/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import useWmsConfig from '@/composables/store/use-wms-config'
import useVisible from '@compos/use-visible'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
// import amountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import purchaseDetailButton from '@/components-system/wms/purchase-detail-button/index.vue'
import reviewConvenientOperate from '@/components-system/common/review-convenient-operate.vue'
import ReviewConfirmButton from '@/components-system/common/review-confirm-button.vue'

import inspectionReturnInfo from '@/views/wms/material-inbound/raw-material/components/inspection-return-info.vue'
import logisticsForm from '@/views/wms/material-inbound/raw-material/components/logistics-form.vue'
// import priceSetColumns from '@/views/wms/material-inbound/raw-material/components/price-set-columns.vue'
import warehouseSetColumns from '@/views/wms/material-inbound/raw-material/components/warehouse-set-columns.vue'
import titleAfterInfo from '@/views/wms/material-inbound/raw-material/components/title-after-info.vue'
import checkPermission from '@/utils/system/check-permission'

const emit = defineEmits(['refresh', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  data: {
    type: Object,
    default: () => {
      return {}
    }
  }
})

const permission = inject('permission')
// 表格列数据格式转换
const columnsDataFormat = ref([...materialHasAmountColumns, ['remark', 'empty-text']])

const reviewConvenientRef = ref() // 连续审核
const drawerRef = ref() // 当前drawer
const logisticsRef = ref() // 物流表单

const detailLoading = ref(false) // 详情loading
const passedLoading = ref(false) // 提交loading
const returnedLoading = ref(false) // 退回loading

const expandRowKeys = ref([]) // 展开
const amount = ref() // 金额变化
const form = ref({})
const operateRecordNumber = ref(0) // 操作记录条数
const reviewNext = ref(false) // 当前记录审核完成后，直接审核下一条
const pendingReviewIdList = ref([]) // 待审核列表
// const currentReviewIndex = ref(0) // 当前审核下标
const currentInboundId = ref() // 当前id

const { inboundFillWayCfg } = useWmsConfig()

// 可填写金额（统一为入库填写，取消后台配置）
// const fillableAmount = ref(false)
const fillableAmount = computed(() =>
  inboundFillWayCfg.value ? inboundFillWayCfg.value.amountFillWay === inboundFillWayEnum.REVIEWING.V : false
)

// 显示金额相关信息（由采购填写的信息）
const showAmount = computed(() => checkPermission(permission.showAmount) || fillableAmount.value)

// 可填写仓库信息（统一为入库填写，取消后台配置）
const fillableWarehouse = ref(false)
// const fillableWarehouse = computed(() =>
//   inboundFillWayCfg.value ? inboundFillWayCfg.value.warehouseFillWay === inboundFillWayEnum.REVIEWING.V : false
// )

// 显示物流信息
const fillableLogistics = computed(() => order.value.logisticsPayerType === logisticsPayerEnum.DEMAND.V && fillableAmount.value)
// 是否“甲供”
const boolPartyA = computed(() => form.value?.supplyType === orderSupplyTypeEnum.PARTY_A.V)
// 采购合同信息
const order = computed(() => form.value.purchaseOrder || {})
// 申购单信息
// const requisitions = computed(() => form.value.requisitions || {})
// 表单禁止操作
const formDisabled = computed(() => passedLoading.value || returnedLoading.value)
// 标题
const drawerTitle = computed(() =>
  detailLoading.value
    ? `入库单`
    : `入库单：${form.value.serialNumber}（ ${order.value.supplier ? order.value.supplier.name : '无供应商'} ）`
)
// 在列中显示次要信息
const showTableColumnSecondary = computed(() => {
  // 非甲供订单，显示项目和申购单 或者仓库时
  const unshow1 = showAmount.value && !boolPartyA.value && ((order.value.projects && order.value.requisitionsSN) || fillableWarehouse.value)
  // 甲供订单，显示项目和申购单以及仓库时
  const unshow2 = showAmount.value && boolPartyA.value && order.value.projects && order.value.requisitionsSN && fillableWarehouse.value
  return !(unshow1 || unshow2)
})

// 仓管填写的信息（工厂及仓库）
const warehouseRules = {
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
  warehouseId: [{ required: true, message: '请选择仓库', trigger: 'change' }]
}

// 采购填写的信息（金额、申购单）
const amountRules = {
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  amount: [{ required: true, message: '请填写金额', trigger: 'blur' }]
}

// 项目
// const projectRules = {
//   projectId: [{ required: true, message: '请选择项目', trigger: 'change' }]
// }

const tableRules = computed(() => {
  const rules = {}
  // 甲供不填写金额方面的信息
  if (fillableAmount.value && !boolPartyA.value) {
    Object.assign(rules, amountRules)
    // if (isNotBlank(order.value.projects)) {
    //   Object.assign(rules, projectRules)
    // }
  }
  if (fillableWarehouse.value) Object.assign(rules, warehouseRules)
  return rules
})

const { crud } = regExtra()

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-inbound-application-review-form',
    extraBox: ['.el-drawer__header', '.approval-comments', '.logistics-form-content', '.inspection-return-info'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  () => computed(() => !detailLoading.value)
)

// 同上的选项与值
const ditto = new Map([
  ['requisitionsSN', -1],
  ['projectId', -1],
  ['factoryId', -1],
  ['warehouseId', -1]
])

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, closeHook: closeHook })

// 表格校验
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules, ditto })

// 监听数据变化
watch(
  () => props.data,
  async (data) => {
    init()
    if (data && data.id) {
      currentInboundId.value = data.id
      detailLoading.value = true
      await fetchPendingReviewIdList()
      fetchDetail(data.id)
    }
  },
  { immediate: true, deep: true }
)

// 初始化
function init() {
  detailLoading.value = false // 详情loading
  returnedLoading.value = false // 退回loading
  passedLoading.value = false // 通过loading
  expandRowKeys.value = [] // 展开
  amount.value = undefined // 金额变化
  form.value = {} // 表单重置
}

// 处理连续审核
function handleConvenientChange(id) {
  init()
  fetchDetail(id)
}

// 获取待审核入库单id列表
async function fetchPendingReviewIdList() {
  pendingReviewIdList.value = await getPendingReviewIdList(crud.query)
}

// 加载详情
async function fetchDetail(id) {
  if (!id) return
  try {
    detailLoading.value = true
    const data = await detail(id)
    data.logistics = {}
    form.value = await detailFormat(data)
  } catch (error) {
    console.log('加载失败', error)
  } finally {
    detailLoading.value = false
  }
}

// 详情格式转换
async function detailFormat(form) {
  if (form.purchaseOrder?.projects) {
    form.purchaseOrder.projectIds = form.purchaseOrder.projects.filter((v) => v !== v.id)
  }
  form.requisitions = {}
  if (isNotBlank(form.requisitionsList)) {
    form.requisitionsList.forEach((item) => {
      form.requisitions[item.serialNumber] = item
    })
  }
  await setSpecInfoToList(form.list)
  form.list = await numFmtByBasicClass(form.list, {
    toSmallest: false,
    toNum: true
  })
  form.list.forEach((item) => (item.sourceRequisitionsSN = item.requisitionsSN))
  form.originList = deepClone(form.list)
  form.list = form.originList.filter((v) => v.qualityTestingEnum & (inspectionStatusEnum.ALL_PASS.V | inspectionStatusEnum.NO.V))
  form.returnList = form.originList.filter((v) => v.qualityTestingEnum & inspectionStatusEnum.ALL_REFUSE.V)
  setDitto(form.list) // 在list变化时设置同上
  return form
}

// 表单提交前校验
async function validate(form) {
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

// 表单提交数据清理
async function submitFormFormat(form) {
  cleanUpData(form.list)
  form.list = await numFmtByBasicClass(form.list, { toSmallest: true, toNum: true })
  return form
}

// 通过提交
async function passed() {
  try {
    passedLoading.value = true
    const validResult = await validate(form.value)
    if (!validResult) return
    const copyForm = deepClone(form.value)
    const nForm = await submitFormFormat(copyForm)
    await reviewPassed(nForm)
    handleAfterSubmit()
  } catch (error) {
    console.log('通过提交', error)
  } finally {
    passedLoading.value = false
  }
}

// 退回
async function returned() {
  try {
    returnedLoading.value = true
    const data = {
      id: form.value.id,
      approvalComments: form.value.approvalComments
    }
    await reviewReturned(data)
    handleAfterSubmit()
  } catch (error) {
    console.log('退回', error)
  } finally {
    returnedLoading.value = false
  }
}

// 提交后处理
function handleAfterSubmit() {
  try {
    ++operateRecordNumber.value
    // 继续审核
    if (reviewNext.value && reviewConvenientRef.value) {
      reviewConvenientRef.value.removeCurrent()
    } else {
      handleClose()
    }
  } catch (error) {
    console.log('审核提交后', error)
    handleClose()
  }
}

// 关闭钩子
function closeHook() {
  // 审核数量>0 则刷新
  if (operateRecordNumber.value) {
    emit('refresh')
  }
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
//   if (!form.value.list) return
//   amount.value = toFixed(
//     form.value.list.reduce((sum, cur) => {
//       const value = Number(cur.amount)
//       if (!isNaN(value)) {
//         return sum + cur.amount
//       } else {
//         return sum
//       }
//     }, 0),
//     DP.YUAN
//   )
// }

function handleUnitPriceChange(val, row) {
  const dp = getDP(val)
  if (dp > 10) {
    row.unitPrice = toPrecision(val, 10)
    val = row.unitPrice
  }
  row.amount = isNotBlank(val) ? toPrecision(val * row.mete, DP.YUAN) : undefined
}

// 处理金额变化
function handleAmountChange(val, row) {
  row.unitPrice = isNotBlank(val) ? toPrecision(val / row.mete, 10) : undefined
}

// 合计
function getSummaries(param) {
  return tableSummary(param, { props: ['quantity', 'mete', 'amount'] })
}
</script>

<style lang="scss" scoped>
.raw-mat-inbound-application-review-form {
  .form {
    height: 100%;
    width: 100%;
    min-height: 400px;
    position: relative;
  }

  .logistics-form-content {
    padding: 0 30px;
    position: absolute;
    bottom: 0;
    left: 0;
  }

  .convenient-operation {
    margin-right: 10px;
  }
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
}
</style>
