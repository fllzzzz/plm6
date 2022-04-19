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
    :data-format="dataFormat"
    style="width: 100%"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" :show-overflow-tooltip="true" label="供应商" align="center" min-width="100"/>
    <el-table-column v-if="columns.visible('amount')" key="amount" prop="amount" :show-overflow-tooltip="true" label="交易总额(元)" min-width="100"/>
    <el-table-column v-if="columns.visible('paymentAmount')" key="paymentAmount" prop="paymentAmount" :show-overflow-tooltip="true" label="已付金额(元)" min-width="100" />
    <el-table-column v-if="columns.visible('unPaymentAmount')" key="unPaymentAmount" prop="unPaymentAmount" :show-overflow-tooltip="true" label="未付金额(元)" min-width="100"/>
    <el-table-column v-if="columns.visible('paymentRate')" key="payRate" prop="payRate" :show-overflow-tooltip="true" label="付款率" min-width="100">
      <template #default="{ row }">
        <span>{{ row.paymentRate }}%</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" :show-overflow-tooltip="true" label="已收票(元)" min-width="100"/>
    <el-table-column v-if="columns.visible('unInvoiceAmount')" key="unInvoiceAmount" prop="unInvoiceAmount" :show-overflow-tooltip="true" label="未收票(元)" min-width="100"/>
    <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" :show-overflow-tooltip="true" label="收票率" min-width="100">
      <template #default="{ row }">
        <span>{{ row.invoiceRate }}%</span>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/payable'
import { ref } from 'vue'
import { contractSupplierPayablePM as permission } from '@/page-permission/contract'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '应付汇总',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const dataFormat = ref([
  ['paymentRate', ['to-fixed', 2]],
  ['invoiceRate', ['to-fixed', 2]],
  ['amount', 'to-thousand'],
  ['paymentAmount', 'to-thousand'],
  ['unPaymentAmount', 'to-thousand'],
  ['invoiceAmount', 'to-thousand'],
  ['unInvoiceAmount', 'to-thousand']
])

const { maxHeight } = useMaxHeight({
  wrapperBox: '.supplierPayable',
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    // 付款比例
    v.paymentRate = v.amount ? (v.paymentAmount || 0) / (v.amount || 0) * 100 : 0
    // 收票比例
    v.invoiceRate = v.amount ? (v.invoiceAmount || 0) / (v.amount || 0) * 100 : 0
  })
}

CRUD.HOOK.beforeRefresh = () => {
  if (crud.query.createTime.length > 0) {
    crud.query.startDate = crud.query.createTime[0]
    crud.query.endDate = crud.query.createTime[1]
  } else {
    crud.query.startDate = undefined
    crud.query.endDate = undefined
  }
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
