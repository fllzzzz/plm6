<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
    ref="tableRef"
    v-loading="crud.loading"
    :data="crud.data"
    :empty-text="crud.emptyText"
    :max-height="maxHeight"
    style="width: 100%"
    :data-format="dataFormat"
    :showEmptySymbol="false"
    :stripe="false"
    show-summary
    :summary-method="getSummaries"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('paymentDate')" key="paymentDate" prop="paymentDate" label="付款日期" align="center" width="100" />
    <el-table-column v-if="columns.visible('paymentUnit')" key="paymentUnit" prop="paymentUnit" :show-overflow-tooltip="true" label="付款单位" align="center" min-width="140" />
    <el-table-column v-if="columns.visible('receivingUnit')" key="receivingUnit" prop="receivingUnit" :show-overflow-tooltip="true" label="收款单位" align="center" min-width="140" />
    <el-table-column v-if="columns.visible('actuallyPaymentAmount')" key="actuallyPaymentAmount" prop="actuallyPaymentAmount" label="实付金额" align="right" min-width="100" />
    <el-table-column key="attachments" prop="attachments" label="附件" align="center" :show-overflow-tooltip="true" min-width="160">
      <template #header>
        <el-tooltip effect="light" :content="`双击可预览附件`" placement="top">
          <div>
            <span>附件</span>
            <i class="el-icon-info" />
          </div>
        </el-tooltip>
      </template>
      <template v-slot="scope">
        <template v-if="scope.row.attachments && scope.row.attachments.length>0">
          <div v-for="item in scope.row.attachments" :key="item.id">
            <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{item.name}}</div>
          </div>
        </template>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="所属项目或订单" align="center" min-width="180">
      <template v-slot="scope">
        <div v-if="scope.row.serialNumber">{{ scope.row.serialNumber }}</div>
        <div v-else>
          <div>{{scope.row.projectNameList?scope.row.projectNameList.join(','):''}}</div>
          <div>{{scope.row.serialNumberList?scope.row.serialNumberList.join(','):''}}</div>
        </div>
      </template>
    </el-table-column>
  </common-table>
  <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/payment-ledger/payment'
import { ref } from 'vue'

import { contractSupplierPaymentLedgerPM } from '@/page-permission/contract'
import { tableSummary } from '@/utils/el-extra'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const { decimalPrecision } = useDecimalPrecision()
const permission = contractSupplierPaymentLedgerPM.payment

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const pdfShow = ref(false)
const currentId = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '付款台账',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['contractAmount'],
    hasPagination: true
  },
  tableRef
)

const dataFormat = ref([
  ['actuallyPaymentAmount', ['to-thousand', decimalPrecision.contract]],
  ['paymentDate', ['parse-time', '{y}-{m}-{d}']]
])

const { maxHeight } = useMaxHeight({
  wrapperBox: '.paymentLedger',
  paginate: true,
  extraHeight: 120
})

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

CRUD.HOOK.beforeRefresh = () => {
  if (crud.query.createTime?.length > 0) {
    crud.query.startDate = crud.query.createTime[0]
    crud.query.endDate = crud.query.createTime[1]
  } else {
    crud.query.startDate = undefined
    crud.query.endDate = undefined
  }
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['actuallyPaymentAmount', decimalPrecision.contract]],
    toThousandFields: ['actuallyPaymentAmount']
  })
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>
