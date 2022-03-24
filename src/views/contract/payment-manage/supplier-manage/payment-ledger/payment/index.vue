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
    return-source-data
    :showEmptySymbol="false"
    :stripe="false"
    :summary-method="getSummaries"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" :show-overflow-tooltip="true" label="付款单位">
      <template v-slot="scope">
        <span>{{ scope.row.businessType?scope.row.businessType:'-'}}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" :show-overflow-tooltip="true" label="收款单位">
      <template v-slot="scope">
        <span>{{ scope.row.businessType?scope.row.businessType:'-'}}</span>
      </template>
    </el-table-column>
     <el-table-column v-if="columns.visible('collectionDate')" key="collectionDate" prop="collectionDate" label="付款日期" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.collectionDate? parseTime(scope.row.collectionDate,'{y}-{m}-{d}'): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('haveCollectionAmount')" key="haveCollectionAmount" prop="haveCollectionAmount" label="付款额(元)" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.haveCollectionAmount && scope.row.haveCollectionAmount>0? toThousand(scope.row.haveCollectionAmount): scope.row.haveCollectionAmount }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('collectionUnit')" key="collectionUnit" prop="collectionUnit" :show-overflow-tooltip="true" label="所属项目或订单" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.collectionUnit }}</div>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/payment-ledger/payment'
import { ref } from 'vue'
import { contractSupplierPaymentLedgerPM } from '@/page-permission/contract'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
// import { projectNameFormatter } from '@/utils/project'

const permission = contractSupplierPaymentLedgerPM.collection

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
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

const { maxHeight } = useMaxHeight({
  wrapperBox: '.paymentLedger',
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.beforeRefresh = () => {
  if (crud.query.createTime.length > 0) {
    crud.query.startDate = crud.query.createTime[0]
    crud.query.endDate = crud.query.createTime[1]
  } else {
    crud.query.startDate = undefined
    crud.query.endDate = undefined
  }
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (column.property === 'haveCollectionAmount') {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sums[index] = sums[index].toFixed(2)
      }
    }
  })
  return sums
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
