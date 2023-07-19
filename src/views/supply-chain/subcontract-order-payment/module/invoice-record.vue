<template>
  <common-drawer
    ref="drawerRef"
    title="收票记录"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="invoice-record"
    size="80%"
  >
    <template #titleAfter>
      <el-tag type="success" effect="plain" size="medium">分包订单：{{detailInfo.serialNumber}}</el-tag>
      <el-tag type="warning" effect="plain" size="medium">分包单位：{{detailInfo.supplierName}}</el-tag>
    </template>
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="props.permission?.invoice.print"
          api-key="purchaseInvoiceRecord"
          :params="{ ...params }"
          size="mini"
          type="warning"
        />
      </div>
    </template>
    <template #content>
      <common-table :data="list" v-loading="tableLoading" show-summary :summary-method="getSummaries" :data-format="dataFormat" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="receiveInvoiceDate" label="收票日期" align="center" width="100" show-overflow-tooltip />
        <el-table-column prop="invoiceAmount" label="发票额" align="right" min-width="120" show-overflow-tooltip />
        <el-table-column label="大写" align="center" min-width="150" show-overflow-tooltip>
          <template #default="{ row }">
          <span>{{ digitUppercase(row?.sourceRow?.invoiceAmount) }}</span>
        </template>
      </el-table-column>
        <el-table-column prop="invoiceType" label="发票类型" align="center" width="110" show-overflow-tooltip />
        <el-table-column prop="taxRate" label="税率" align="center" width="70" show-overflow-tooltip>
          <template #default="{ row }">
            <span>{{ row.taxRate }}<span v-if="row.taxRate!=='-'">%</span></span>
          </template>
        </el-table-column>
        <el-table-column prop="branchCompanyName" label="购方单位" align="center" min-width="140" show-overflow-tooltip />
        <el-table-column prop="supplierName" label="销售单位" align="center" min-width="140" show-overflow-tooltip />
        <el-table-column prop="invoiceSerialNumber" label="发票编号" align="center" min-width="100" show-overflow-tooltip />
        <el-table-column prop="applyUserName" label="办理人" align="center" min-width="100" show-overflow-tooltip />
        <el-table-column prop="auditUserName" label="审核人" align="center" min-width="100" show-overflow-tooltip />
      </common-table>
      <!--分页组件-->
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
import { invoiceRecord } from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'

import { supplierPayTypeEnum } from '@enum-ms/contract'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { digitUppercase, getDP, toThousand } from '@/utils/data-type/number'
import { tableSummary } from '@/utils/el-extra'

import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const emit = defineEmits(['update:modelValue'])

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

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

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
  }
)

const list = ref([])
const drawerRef = ref()
const tableLoading = ref(false)
const dataFormat = computed(() => {
  return [
    ['invoiceType', ['parse-enum', invoiceTypeEnum]],
    ['receiveInvoiceDate', ['parse-time', '{y}-{m}-{d}']],
    ['invoiceAmount', ['to-thousand', decimalPrecision.value.supplyChain]],
    ['taxRate', ['to-fixed', 2]]
  ]
})

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.invoice-record',
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
    props: [['invoiceAmount', decimalPrecision.value.supplyChain]]
  })
  const num = summary[2]
  if (num) {
    const dp = getDP(num)
    summary[3] = digitUppercase(num)
    summary[2] = toThousand(num, dp)
  }
  return summary
}

// 获取收票记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await invoiceRecord({ ...params.value, ...queryPage })
    _list = content
    setTotalPage(totalElements)
  } catch (error) {
    console.log('获取收票记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>
