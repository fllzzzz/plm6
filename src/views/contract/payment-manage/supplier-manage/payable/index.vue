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
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" :show-overflow-tooltip="true" label="供应商" align="center" min-width="100">
      <template v-slot="scope">
        <div>{{ scope.row.supplierName }}</div>
      </template>
    </el-table-column>
     <el-table-column v-if="columns.visible('amount')" key="amount" prop="amount" :show-overflow-tooltip="true" label="交易总额(元)" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.amount? toThousand(scope.row.amount): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentAmount')" key="paymentAmount" prop="paymentAmount" :show-overflow-tooltip="true" label="已付金额(元)" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.paymentAmount? toThousand(scope.row.paymentAmount): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('unPaymentAmount')" key="unPaymentAmount" prop="unPaymentAmount" :show-overflow-tooltip="true" label="未付金额(元)" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.unPaymentAmount? toThousand(scope.row.unPaymentAmount): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('payRate')" key="payRate" prop="payRate" :show-overflow-tooltip="true" label="付款率" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.payRate? scope.row.payRate+'%': '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" :show-overflow-tooltip="true" label="已开票(元)" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.invoiceAmount? toThousand(scope.row.invoiceAmount): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('unInvoiceAmount')" key="unInvoiceAmount" prop="unInvoiceAmount" :show-overflow-tooltip="true" label="未开票(元)" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.unInvoiceAmount? toThousand(scope.row.unInvoiceAmount): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" :show-overflow-tooltip="true" label="开票率" min-width="100">
      <template v-slot="scope">
        <span>{{ scope.row.invoiceRate? scope.row.invoiceRate+'%': '' }}</span>
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
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import { toThousand } from '@data-type/number'

// crud交由presenter持有
const permission = {
  get: ['supplierPayable:get']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
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

const { maxHeight } = useMaxHeight({
  wrapperBox: '.supplierPayable',
  paginate: true,
  extraHeight: 40
})

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
