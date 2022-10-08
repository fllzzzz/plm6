<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    :content-loading="detailLoading"
    :before-close="handleClose"
    :title="drawerTitle"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-reject-application-review-form"
  >
    <template #titleAfter>
      <title-after-info :order="order" :detail="detail" />
    </template>
    <template #titleRight>
      <review-convenient-operate
        ref="reviewConvenientRef"
        v-if="pendingReviewIdList && pendingReviewIdList.length > 1"
        v-model="currentRecordId"
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
      <unfreeze-info class="unfreeze-info" v-if="detail.boolHasUnfreeze" :basic-class="detail.basicClass" :list="detail.unfreezeList" />
      <el-form class="form" :model="form" :disabled="formDisabled">
        <common-table
          :data="detail.list"
          :data-format="columnsDataFormat"
          :max-height="maxHeight"
          show-summary
          :summary-method="getSummaries"
          :expand-row-keys="expandRowKeys"
          row-key="id"
        >
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="detail.basicClass" />
          <!-- 次要信息 -->
          <material-secondary-info-columns :basic-class="detail.basicClass" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns :basic-class="detail.basicClass" />
          <!-- 价格信息 -->
          <template v-if="showAmount">
            <amount-info-columns v-if="!boolPartyA" />
          </template>
          <warehouse-info-columns show-project show-monomer show-area />
        </common-table>
        <p class="remark">
          <span class="label-after">备注</span>
          <span v-empty-text="detail.remark" />
        </p>
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
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { getPendingReviewIdList, detail as getDetail, reviewPassed, reviewReturned } from '@/api/wms/material-reject/raw-material/review'
import { computed, ref, defineEmits, defineProps, watch, inject } from 'vue'
import { orderSupplyTypeEnum } from '@enum-ms/wms'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import checkPermission from '@/utils/system/check-permission'
import { materialHasAmountColumns } from '@/utils/columns-format/wms'

import { regExtra } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import purchaseDetailButton from '@/components-system/wms/purchase-detail-button/index.vue'
import reviewConvenientOperate from '@/components-system/common/review-convenient-operate.vue'
import ReviewConfirmButton from '@/components-system/common/review-confirm-button.vue'
import amountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'

import unfreezeInfo from '@/views/wms/material-freeze/raw-material/components/unfreeze-info.vue'
import titleAfterInfo from '@/views/wms/material-reject/raw-material/components/title-after-info.vue'
import useContinuousReview from '@/composables/use-continuous-review'

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
const drawerRef = ref() // 当前drawer
const detailLoading = ref(false) // 详情loading
const expandRowKeys = ref([]) // 展开

// 表单禁止操作
const formDisabled = computed(() => passedLoading.value || returnedLoading.value)
const form = ref({})
const detail = ref({})

// 表格列数据格式转换
const columnsDataFormat = ref([...materialHasAmountColumns])
const { crud } = regExtra()

// 采购订单信息
const order = computed(() => detail.value.purchaseOrder || {})
// 是否有显示金额权限
const showAmount = computed(() => checkPermission(permission.showAmount))
// 是否甲供订单
const boolPartyA = computed(() => order.value.supplyType === orderSupplyTypeEnum.PARTY_A.V)
// 标题
const drawerTitle = computed(() =>
  detailLoading.value ? `退货单` : `退货单：${detail.value.serialNumber}（ ${order.value.supplier ? order.value.supplier.name : ''} ）`
)

// 表格高度处理

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-reject-application-review-form',
    extraBox: ['.el-drawer__header', '.approval-comments', '.unfreeze-info'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300
  },
  () => computed(() => !detailLoading.value)
)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, closeHook: closeHook })

// 连续审核
const {
  reviewNext,
  currentRecordId,
  pendingReviewIdList,
  operateRecordNumber,
  returnedLoading,
  passedLoading,
  reviewConvenientRef,
  reviewInit,
  detailInit,
  passed,
  returned,
  handleConvenientChange
} = useContinuousReview({
  detailMethod: fetchDetail,
  passedMethod: () => reviewPassed(form.value),
  returnedMethod: () => reviewReturned(form.value),
  pendingListMethod: () => getPendingReviewIdList(crud.query),
  detailInitCallBack: detailInitCallBack,
  closeDlg: handleClose
})

// 监听数据变化
watch(
  () => props.data,
  async (data) => {
    detailInit()
    if (data && data.id) {
      currentRecordId.value = data.id
      detailLoading.value = true
      reviewInit()
      fetchDetail(data.id)
    }
  },
  { immediate: true, deep: true }
)

// 初始化
function detailInitCallBack() {
  detailLoading.value = false // 详情loading
  expandRowKeys.value = [] // 展开
}

// 加载详情
async function fetchDetail(id) {
  if (!id) return
  try {
    detailLoading.value = true
    const data = await getDetail(id)
    detail.value = await detailFormat(data)
    form.value = {
      id: detail.value.id
    }
  } catch (error) {
    console.log('加载失败', error)
  } finally {
    detailLoading.value = false
  }
}

// 详情格式转换
async function detailFormat(data) {
  // 当前数据
  await setSpecInfoToList(data.list)
  await numFmtByBasicClass(data.list)
  return data
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
  return tableSummary(param, { props: ['quantity', 'mete'] })
}
</script>

<style lang="scss" scoped>
.raw-mat-reject-application-review-form {
  .form {
    height: 100%;
    width: 100%;
    min-height: 400px;
    position: relative;
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
  .remark {
    font-size: 14px;
    :first-child {
      font-weight: 700;
    }
  }
}
</style>
