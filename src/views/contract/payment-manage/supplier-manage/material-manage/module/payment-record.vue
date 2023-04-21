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
      <el-tag v-if="detailInfo.serialNumber" type="success" effect="plain" size="medium">采购合同编号：{{detailInfo.serialNumber}}</el-tag>
      <el-tag v-else type="warning" effect="plain" size="medium">供应商：{{detailInfo.supplierName}}</el-tag>
      <el-tag>累计支付：{{detailInfo.paymentAmount}}</el-tag>
    </template>
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="props.permission?.print"
          api-key="purchasePaymentRecord"
          :params="{ supplierId: props.detailInfo.supplierId,auditStatus: auditTypeEnum.PASS.V }"
          size="mini"
          type="warning"
        />
      </div>
    </template>
    <template #content>
      <div class="head-container">
        <el-date-picker
          v-model="query.date"
          type="daterange"
          range-separator=":"
          size="small"
          value-format="x"
          class="filter-item date-item"
          start-placeholder="开始时间"
          end-placeholder="结束时间"
          style="width: 240px"
          @change="handleDateChange"
        />
      </div>
      <common-table :data="list" v-loading="tableLoading" show-summary :summary-method="getSummaries" :data-format="dataFormat" :max-height="maxHeight">
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="paymentDate" prop="paymentDate" label="付款日期" align="center" width="90" />
      <el-table-column key="actuallyPaymentAmount" prop="actuallyPaymentAmount" label="支付金额" align="right" min-width="80">
        <template #default="{ row }">
          <template v-if="row.attachments && row.attachments.length>0">
            <div v-for="item in row.attachments" :key="item.id">
              <div style="cursor:pointer;color:#409eff;" @dblclick="attachmentView(item)">{{toThousand(row.actuallyPaymentAmount)}}</div>
            </div>
          </template>
          <template v-else>{{toThousand(row.actuallyPaymentAmount)}}</template>
        </template>
      </el-table-column>
      <el-table-column key="paymentReasonId" prop="paymentReasonId" label="付款事由" align="center" width="100">
          <template #default="{ row }">
         <div>{{ dict?.label?.['payment_reason']?.[row.paymentReasonId] }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentUnit" prop="paymentUnit" label="付款单位" align="center" min-width="140" show-overflow-tooltip />
      <el-table-column key="paymentBank" prop="paymentBank" show-overflow-tooltip label="付款银行" align="center" min-width="130">
        <template #default="{ row }">
          <div>{{row.paymentBank}}{{row.paymentBankAccount?'【'+row.paymentBankAccount+'】':''}}</div>
        </template>
      </el-table-column>
      <el-table-column key="receivingUnit" prop="receivingUnit" label="收款单位" align="center" min-width="140" show-overflow-tooltip />
      <!-- <el-table-column key="receivingBank" prop="receivingBank" label="收款银行" align="center" min-width="140" show-overflow-tooltip>
        <template #default="{ row }">
          <div>{{row.receivingBank}}{{row.receiveBankAccount?'【'+row.receiveBankAccount+'】':''}}</div>
        </template>
      </el-table-column> -->
      <el-table-column key="applyUserName" prop="applyUserName" label="办理人" align="center" width="100px" />
      <el-table-column key="auditUserName" prop="auditUserName" label="审核人" align="center" width="100px" />
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
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { paymentRecord } from '@/api/supply-chain/purchase-reconciliation-manage/jd-payment-ledger'
import { defineEmits, defineProps, ref, computed, watch } from 'vue'

import { auditTypeEnum } from '@enum-ms/contract'
import { digitUppercase, getDP, toThousand } from '@/utils/data-type/number'
import { tableSummary } from '@/utils/el-extra'

import useVisible from '@/composables/use-visible'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import useDict from '@compos/store/use-dict'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const emit = defineEmits(['success', 'update:modelValue'])

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })
const query = ref({})
const pdfShow = ref(false)
const currentId = ref()

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
  },
  queryDate: {
    type: Object,
    default: () => {}
  }
})

// 请求参数
const params = computed(() => {
  const data = {
    supplierId: props.detailInfo.supplierId,
    auditStatus: auditTypeEnum.PASS.V,
    ...query.value
  }
  return data
})

watch(
  visible,
  (val) => {
    if (val) {
      query.value = {
        date: props.queryDate?.startDate && props.queryDate?.endDate ? [props.queryDate.startDate, props.queryDate.endDate] : [],
        startDate: props.queryDate?.startDate,
        endDate: props.queryDate?.endDate
      }
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
  ['applyAmount', 'to-thousand'],
  // ['actuallyPaymentAmount', 'to-thousand'],
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
    props: ['applyAmount', 'actuallyPaymentAmount'],
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

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

// 时间变动
function handleDateChange(val) {
  if (query.value.date && query.value.date.length > 1) {
    query.value.startDate = val[0]
    query.value.endDate = val[1]
  } else {
    query.value.startDate = undefined
    query.value.endDate = undefined
  }
  fetchList()
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
