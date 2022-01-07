<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    :content-loading="detailLoading"
    :before-close="handleClose"
    :title="drawerTitle"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-transfer-application-review-form"
  >
    <template #titleAfter>
      <common-title-info :detail="form" />
    </template>
    <template #titleRight>
      <!-- 连续审核 -->
      <review-convenient-operate
        ref="reviewConvenientRef"
        v-if="pendingReviewIdList && pendingReviewIdList.length > 1"
        v-model="currentInboundId"
        v-model:consequent="reviewNext"
        :list="pendingReviewIdList"
        @change="handleConvenientChange"
        class="convenient-operation"
      />
      <!-- 审核按钮 -->
      <review-confirm-button
        :passed-loading="passedLoading"
        :returned-loading="returnedLoading"
        :passed-fn="passed"
        :returned-fn="returned"
      />
    </template>
    <template #content>
      <el-form ref="formRef" class="form" :model="form" :rules="rules" :disabled="formDisabled" label-position="left" size="mini">
        <unfreeze-info class="unfreeze-info" v-if="form.boolHasUnfreeze" :basic-class="form.basicClass" :list="form.unfreezeList" />
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
              <expand-secondary-info v-if="showAmount" :basic-class="detail.basicClass" :row="row" show-brand>
                <p>
                  备注：<span v-empty-text>{{ row.remark }}</span>
                </p>
              </expand-secondary-info>
              <p v-else>
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
          <!-- 甲供设置 -->
          <set-party-a-info v-if="showAmount && form && form.list" :form="form" />
          <!-- 仓库信息 -->
          <warehouse-info-columns show-project />
        </common-table>
        <div class="flex-rss footer-info">
          <el-form-item v-if="showPriceSet" class="invoice-type-item" label="发票及税率" prop="invoiceType" label-width="95px">
            <invoice-type-select
              class="input-underline"
              v-model:invoiceType="form.invoiceType"
              v-model:taxRate="form.taxRate"
              :classification="form.basicClass"
              default
            />
          </el-form-item>
          <el-input
            class="approval-comments"
            v-model="form.approvalComments"
            :rows="2"
            maxLength="1000"
            type="textarea"
            show-word-limit
            placeholder="审核意见"
            style="margin-top: 5px"
          />
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { getPendingReviewIdList, detail, reviewPassed, reviewReturned } from '@/api/wms/transfer/raw-mat-application-review'
import { computed, ref, defineEmits, defineProps, watch, nextTick } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { deepClone } from '@/utils/data-type'
import { partyAMatTransferEnum, transferTypeEnum } from '@/utils/enum/modules/wms'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'

import { regExtra } from '@/composables/use-crud'
import useTableValidate from '../composables/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import ReviewConvenientOperate from '@/components-system/common/review-convenient-operate.vue'
import ReviewConfirmButton from '@/components-system/common/review-confirm-button.vue'
import invoiceTypeSelect from '@/components-system/base/invoice-type-select.vue'
import unfreezeInfo from './unfreeze-info.vue'
import commonTitleInfo from './common-title-info.vue'
import setPartyAInfo from './set-party-a-info.vue'

const emit = defineEmits(['refresh', 'update:visible'])

const props = defineProps({
  visible: {
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
const formRef = ref() // 当前form

const detailLoading = ref(false) // 详情loading
const passedLoading = ref(false) // 提交loading
const returnedLoading = ref(false) // 退回loading

const expandRowKeys = ref([]) // 展开
const form = ref({}) // 提交表单
const operateRecordNumber = ref(0) // 操作记录条数
const reviewNext = ref(false) // 当前记录审核完成后，直接审核下一条
const pendingReviewIdList = ref([]) // 待审核列表
const currentInboundId = ref() // 当前id
const showAmount = ref(false) // 显示金额，只有“买入甲供材料才需要填写金额”

// 表单禁止操作
const formDisabled = computed(() => passedLoading.value || returnedLoading.value)
// 标题
const drawerTitle = computed(() => (detailLoading.value ? `调拨单：` : `调拨单：${form.value.serialNumber || ''}`))

const { crud } = regExtra()

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-transfer-application-review-form',
    extraBox: ['.el-drawer__header', '.approval-comments', '.unfreeze-info'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300
  },
  () => computed(() => !detailLoading.value)
)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook, closeHook: closeHook })

// 表格校验
const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate()

// 税率设置（价格设置时，可设置）
const showPriceSet = computed(() =>
  form.value.list.some((row) => row.boolPartyA && row.partyATransferType === partyAMatTransferEnum.BUY_IN.V)
)

const validateInvoiceType = (rule, value, callback) => {
  if (form.value.invoiceType || form.value.invoiceType === 0) {
    if (form.value.invoiceType === invoiceTypeEnum.SPECIAL.V && !form.value.taxRate) {
      callback(new Error('请选择税率'))
      return
    } else {
      callback()
    }
  } else {
    callback(new Error('请选择发票及税率'))
    return
  }
}

const invoiceRules = {
  invoiceType: [{ required: true, validator: validateInvoiceType, trigger: 'change' }]
}

const rules = computed(() => {
  nextTick(() => {
    // 清除所有校验，rules
    formRef.value && formRef.value.clearValidate()
  })
  return showPriceSet.value ? invoiceRules : {}
})

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
  await setSpecInfoToList(form.list)
  form.list = await numFmtByBasicClass(form.list, {
    toSmallest: false,
    toNum: true
  })
  // 将甲供材料调拨到其他项目或公共库中时，需要填写金额
  if (form.transferType !== transferTypeEnum.RETURN_PARTY_A.V) {
    let partyANum = 0
    // 遍历判断是否存在甲供材料, 甲供材料需要买入（填写金额）或借用
    form.list.forEach((v) => {
      if (v.boolPartyA) partyANum++
    })
    showAmount.value = partyANum > 0
  } else {
    showAmount.value = false
  }
  return form
}

// 表单提交前校验
async function validate(form) {
  const { validResult, dealList } = tableValidate(form.list)
  if (validResult) {
    form.list = dealList
  }
  const formValidate = await formRef.value.validate()
  return validResult && formValidate
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

function showHook() {
  // 打开时清空表格校验
  if (form.value && form.value.list) {
    cleanUpData(form.value.list)
  }
}

// 关闭钩子
function closeHook() {
  // 审核数量>0 则刷新
  if (operateRecordNumber.value) {
    emit('refresh')
  }
}

// 合计
function getSummaries(param) {
  return tableSummary(param, { props: ['quantity', 'mete', 'amount'] })
}
</script>

<style lang="scss" scoped>
.raw-mat-transfer-application-review-form {
  .el-drawer__header .el-tag {
    min-width: 70px;
    text-align: center;
  }

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

  .invoice-type-item {
    margin: 14px 15px 0 0;
  }
}
</style>
