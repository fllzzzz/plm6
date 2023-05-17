<template>
  <!-- 付款记录 -->
  <common-drawer
    ref="drawerRef"
    title="付款记录"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="payment-record"
    size="100%"
  >
    <template #titleAfter>
      <el-tag type="success" effect="plain" size="medium">分包订单：{{detailInfo.serialNumber}}</el-tag>
      <el-tag type="warning" effect="plain" size="medium">分包单位：{{detailInfo.supplierName}}</el-tag>
    </template>
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="props.permission?.paymentManage.print"
          api-key="purchasePaymentRecord"
          :params="{ ...params }"
          size="mini"
          type="warning"
        />
      </div>
    </template>
    <template #content>
      <common-table :data="list" v-loading="tableLoading" show-summary :summary-method="getSummaries" :data-format="dataFormat" :max-height="maxHeight">
        <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="paymentDate" prop="paymentDate" label="付款日期" align="center" width="90" />
      <el-table-column key="applyAmount" prop="applyAmount" label="申请金额" align="right" min-width="80" />
      <el-table-column key="actuallyPaymentAmount" prop="actuallyPaymentAmount" label="实付金额" align="right" min-width="80" />
      <el-table-column label="大写" align="center" min-width="120" show-overflow-tooltip>
          <template #default="{ row }">
          <div v-if="row.actuallyPaymentAmount">{{ digitUppercase(row?.sourceRow?.actuallyPaymentAmount) }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentReasonId" prop="paymentReasonId" label="付款事由" align="center" width="100">
          <template #default="{ row }">
         <div>{{ dict?.label?.['payment_reason']?.[row.paymentReasonId] }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentMethod" prop="paymentMethod" label="付款方式" align="center" width="100">
          <template #default="{ row }">
          <div>{{ paymentFineModeEnum.VL?.[row.paymentMethod] }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentUnit" prop="paymentUnit" label="付款单位" align="center" min-width="140" show-overflow-tooltip />
      <el-table-column key="paymentBank" prop="paymentBank" show-overflow-tooltip label="付款银行" align="center" min-width="130" />
      <el-table-column key="receivingUnit" prop="receivingUnit" label="收款单位" align="center" min-width="140" show-overflow-tooltip />
      <el-table-column key="applyUserName" prop="applyUserName" label="办理人" align="center" width="100px" />
      <el-table-column key="auditUserName" prop="auditUserName" label="审核人" align="center" width="100px" />
      <el-table-column key="remark" prop="remark" label="备注" align="center" min-width="120" show-overflow-tooltip />
      <el-table-column key="auditStatus" prop="auditStatus" label="审核状态" align="center" width="80">
          <template #default="{ row }">
          <el-tag v-if="row.auditStatus===auditTypeEnum.REJECT.V" type="warning">{{ auditTypeEnum.VL[row.auditStatus] }}</el-tag>
          <el-tag v-else :type="row.auditStatus===auditTypeEnum.PASS.V?'success':''">{{ auditTypeEnum.VL[row.auditStatus] }}</el-tag>
        </template>
      </el-table-column>
      </common-table>
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { paymentRecord } from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { defineEmits, defineProps, ref, computed, watch } from 'vue'

import { auditTypeEnum, supplierPayTypeEnum } from '@enum-ms/contract'
import { digitUppercase, getDP, toThousand } from '@/utils/data-type/number'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { tableSummary } from '@/utils/el-extra'
import { DP } from '@/settings/config'

import useVisible from '@/composables/use-visible'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import useDict from '@compos/store/use-dict'

const emit = defineEmits(['success', 'update:modelValue'])

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

// 请求参数
const params = computed(() => {
  const data = {
    propertyType: supplierPayTypeEnum.SUBCONTRACT.V
  }
  if (props.detailInfo.id) {
    // 订单列表
    data.orderId = props.detailInfo.id
  } else {
    // 汇总列表
    data.supplierId = props.detailInfo.supplierId
  }
  return data
})

watch(
  visible,
  (val) => {
    if (val) {
      fetchList()
    }
  },
  { immediate: true }
)

const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)
const dict = useDict(['payment_reason'])

const dataFormat = ref([
  ['applyAmount', ['to-thousand-ck', 'YUAN']],
  ['actuallyPaymentAmount', ['to-thousand-ck', 'YUAN']],
  ['paymentDate', ['parse-time', '{y}-{m}-{d}']]
])

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.payment-record',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

// 合计
function getSummaries(param) {
  const summary = tableSummary(param, {
    props: [['applyAmount', DP.YUAN], ['actuallyPaymentAmount', DP.YUAN]],
    toThousandFields: ['applyAmount']
  })
  const num = summary[3]
  if (num) {
    const dp = getDP(num)
    summary[4] = digitUppercase(num)
    summary[3] = toThousand(num, dp)
  }
  return summary
}

// 获取付款记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await paymentRecord({ ...params.value, ...queryPage })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取付款记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

</script>
