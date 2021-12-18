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
      <purchase-detail-button v-if="showAmount" :purchase-id="order.id" size="mini" />
      <!-- 审核按钮 -->
      <review-confirm-button
        :passed-loading="passedLoading"
        :returned-loading="returnedLoading"
        :passed-fn="passed"
        :returned-fn="returned"
      />
    </template>
    <template #content>
      <el-form class="form" :disabled="formDisabled">
        <common-table
          :data="form.list"
          :max-height="maxHeight"
          show-summary
          :cell-class-name="wrongCellMask"
          :summary-method="getSummaries"
          :expand-row-keys="expandRowKeys"
          row-key="id"
        >
          <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
            <template #default="{ row }">
              <expand-secondary-info v-if="showAmount" :basic-class="form.basicClass" :row="row" show-brand />
              <p>
                备注：<span v-empty-text>{{ row.remark }}</span>
              </p>
            </template>
          </el-expand-table-column>
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="form.basicClass" fixed="left" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns :basic-class="form.basicClass" />
          <!-- 次要信息 -->
          <material-secondary-info-columns v-if="!showAmount" :basic-class="form.basicClass" />
          <!-- 金额设置 -->
          <price-set-columns
            v-if="showAmount"
            :form="form"
            :order="order"
            :requisitions="requisitions"
            @amount-change="handleAmountChange"
          />
          <template v-else>
            <el-table-column prop="project" label="项目" align="left" min-width="120px" show-overflow-tooltip>
              <template #default="{ row }">
                <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
              </template>
            </el-table-column>
          </template>
          <!-- 仓库设置 -->
          <warehouse-set-columns v-if="showWarehouse" :form="form" />
          <warehouse-info-columns v-else />
        </common-table>
        <el-input
          class="approval-comments"
          v-model="form.approvalComments"
          :rows="2"
          :max="1000"
          type="textarea"
          show-word-limit
          placeholder="审核意见"
          style="margin-top: 5px"
        />
        <!-- 物流信息设置 -->
        <logistics-form
          ref="logisticsRef"
          v-if="showLogistics"
          class="logistics-form-content"
          :disabled="formDisabled"
          :form="form.logistics"
        />
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { getPendingReviewIdList, detail, reviewPassed, reviewReturned } from '@/api/wms/inbound/raw-mat-application-review'
import { computed, ref, defineEmits, defineProps, watch } from 'vue'
import { inboundFillWayEnum, orderSupplyTypeEnum, pickUpModeEnum } from '@enum-ms/wms'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { deepClone, isBlank, isNotBlank, toFixed } from '@/utils/data-type'

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

import logisticsForm from '@/views/wms/inbound-components/logistics-form.vue'
import priceSetColumns from '@/views/wms/inbound-components/price-set-columns.vue'
import warehouseSetColumns from '@/views/wms/inbound-components/warehouse-set-columns.vue'
import titleAfterInfo from '@/views/wms/inbound-components/title-after-info.vue'

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

// 显示金额
const showAmount = computed(() => inboundFillWayCfg.value.amountFillWay === inboundFillWayEnum.REVIEWING.V)
// 显示仓库
const showWarehouse = computed(() => inboundFillWayCfg.value.warehouseFillWay === inboundFillWayEnum.REVIEWING.V)
// 显示物流信息
const showLogistics = computed(() => order.value.pickUpMode === pickUpModeEnum.SELF.V && showAmount.value)
// 是否“甲供”
const boolPartyA = computed(() => order.value.supplyType === orderSupplyTypeEnum.PARTY_A.V)
// 采购订单信息
const order = computed(() => form.value.purchaseOrder || {})
// 申购单信息
const requisitions = computed(() => form.value.requisitions || {})
// 表单禁止操作
const formDisabled = computed(() => passedLoading.value || returnedLoading.value)
// 标题
const drawerTitle = computed(() =>
  detailLoading.value
    ? `入库单：${props.data.serialNumber}`
    : `入库单：${props.data.serialNumber}（ ${order.value.supplier ? order.value.supplier.name : ''} ）`
)

// 仓管填写的信息（工厂及仓库）
const warehouseRules = {
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
  warehouseId: [{ required: true, message: '请选择仓库', trigger: 'change' }]
}

// 采购填写的信息（金额、申购单及项目）
const amountRules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  amount: [{ required: true, message: '请填写金额', trigger: 'blur' }]
}

// 甲供不填写金额方面的信息
const partyAAmountRules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }]
}

const tableRules = computed(() => {
  const rules = {}
  if (showAmount.value) Object.assign(rules, boolPartyA.value ? partyAAmountRules : amountRules)
  if (showWarehouse.value) Object.assign(rules, warehouseRules)
  return rules
})

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-inbound-application-review-form',
    extraBox: ['.el-drawer__header', '.approval-comments', '.logistics-form-content'],
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
  pendingReviewIdList.value = await getPendingReviewIdList()
}

// 加载详情
async function fetchDetail(id) {
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
  form.purchaseOrder.projectIds = form.purchaseOrder.projects.filter((v) => v !== v.id)
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
  if (showLogistics.value && logisticsRef.value) {
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
function handleAmountChange() {
  if (!form.value.list) return
  amount.value = toFixed(
    form.value.list.reduce((sum, cur) => {
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
    ::v-deep(.cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
}
</style>
