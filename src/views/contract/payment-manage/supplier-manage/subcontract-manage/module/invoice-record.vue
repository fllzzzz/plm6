<template>
  <common-drawer
    ref="drawerRef"
    title="收票记录"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="invoice-record"
    size="90%"
  >
    <template #titleAfter>
      <el-tag type="warning" effect="plain" size="medium">分包单位：{{detailInfo.supplierName}}</el-tag>
      <el-tag>累计收票：{{detailInfo.invoiceAmount}}</el-tag>
    </template>
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="props.permission?.print"
          api-key="subcontractInvoiceRecord"
          :params="{ ...params}"
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
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="receiveInvoiceDate" label="收票日期" align="center" width="100" show-overflow-tooltip />
        <el-table-column prop="invoiceAmount" label="票面金额" align="center" min-width="120" show-overflow-tooltip>
           <template #default="{ row }">
          <template v-if="row.attachments && row.attachments.length>0">
            <div v-for="item in row.attachments" :key="item.id">
              <div style="cursor:pointer;color:#409eff;" @dblclick="attachmentView(item)">{{toThousand(row.invoiceAmount)}}</div>
            </div>
          </template>
          <template v-else>{{toThousand(row.invoiceAmount)}}</template>
        </template>
        </el-table-column>
        <!-- <el-table-column label="大写" align="center" min-width="150" show-overflow-tooltip>
          <template #default="{ row }">
          <span>{{ digitUppercase(row?.sourceRow?.invoiceAmount) }}</span>
        </template>
      </el-table-column> -->
        <el-table-column prop="invoiceType" label="发票类型" align="center" width="110" show-overflow-tooltip />
        <el-table-column prop="taxRate" label="税率" align="center" width="70" show-overflow-tooltip>
          <template #default="{ row }">
            <span>{{ row.taxRate }}<span v-if="row.taxRate!=='-'">%</span></span>
          </template>
        </el-table-column>
         <el-table-column prop="amountExcludingTax" label="不含税" align="center" min-width="120" show-overflow-tooltip>
          <template #default="{ row }">
            <span>{{toThousand(row.amountExcludingTax)}}</span>
          </template>
        </el-table-column>
        <el-table-column prop="tax" label="税额" align="center" min-width="120" show-overflow-tooltip>
          <template #default="{ row }">
            <span>{{toThousand(row.tax)}}</span>
          </template>
        </el-table-column>
        <el-table-column prop="invoiceSerialNumber" label="发票编号" align="center" min-width="100" show-overflow-tooltip />
        <el-table-column prop="branchCompanyName" label="购买方" align="center" min-width="140" show-overflow-tooltip />
        <el-table-column prop="supplierName" label="销售方" align="center" min-width="140" show-overflow-tooltip />
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
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { invoiceRecord } from '@/api/supply-chain/subcontract-manage/jd-subcontract-payment'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'

import { auditTypeEnum } from '@enum-ms/contract'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { digitUppercase, getDP, toThousand } from '@/utils/data-type/number'
import { tableSummary } from '@/utils/el-extra'

import useVisible from '@/composables/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

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
  },
  queryDate: {
    type: Object,
    default: () => {}
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })
const query = ref({})
const pdfShow = ref(false)
const currentId = ref()

// 请求参数
const params = computed(() => {
  // 汇总列表
  return {
    supplierId: props.detailInfo.supplierId,
    auditStatus: auditTypeEnum.PASS.V,
    projectId: props.detailInfo.projectId,
    ...query.value
  }
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
const dataFormat = ref([
  ['invoiceType', ['parse-enum', invoiceTypeEnum]],
  ['receiveInvoiceDate', ['parse-time', '{y}-{m}-{d}']],
  ['invoiceAmount', 'to-thousand'],
  ['taxRate', ['to-fixed', 2]]
])

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
    props: ['invoiceAmount']
  })
  const num = summary[2]
  if (num) {
    const dp = getDP(num)
    summary[3] = digitUppercase(num)
    summary[2] = toThousand(num, dp)
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

// 获取收票记录
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await invoiceRecord({ ...params.value, ...queryPage })
    content.map(v => {
      v.amountExcludingTax = v.taxRate ? (v.invoiceAmount / (1 + v.taxRate / 100)).toFixed(2) : v.invoiceAmount
      v.tax = (v.invoiceAmount - v.amountExcludingTax).toFixed(2)
    })
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
