<template>
  <div class="app-container">
    <!--表格渲染-->
    <div>
      <el-tag type="success" size="medium" v-if="currentRow.freight">{{`运输额:${toThousand(currentRow.freight,decimalPrecision.contract)}`}}</el-tag>
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      return-source-data
      :showEmptySymbol="false"
      show-summary
      :summary-method="getSummaries"
      :stripe="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="applyUserName" prop="applyUserName" label="申请人" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.applyUserName? scope.row.applyUserName:'-' }}</div>
        </template>
      </el-table-column>
       <el-table-column key="paymentDate" prop="paymentDate" label="申请日期" align="center" >
        <template v-slot="scope">
          <div>{{ scope.row.paymentDate? parseTime(scope.row.paymentDate,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <!-- <el-table-column key="type" prop="type" label="承运属性" align="center" >
        <template v-slot="scope">
          <div>{{ scope.row.type? logisticsSearchTypeEnum.VL[scope.row.type]: '-' }}</div>
        </template>
      </el-table-column> -->
      <el-table-column key="applyAmount" prop="applyAmount" label="申请金额" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.applyAmount && scope.row.applyAmount>0? toThousand(scope.row.applyAmount,decimalPrecision.contract): scope.row.applyAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditUserName" prop="auditUserName" label="审核人" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.auditUserName? scope.row.auditUserName:'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditTime" prop="auditTime" label="审核日期" align="center">
        <template v-slot="scope">
          <div>{{ scope.row.auditTime? parseTime(scope.row.auditTime,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
    </common-table>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import { get } from '@/api/supply-chain/logistics-payment-manage/logistics-payment'
import { ref, defineProps, watch } from 'vue'

import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { supplierPayTypeEnum, auditTypeEnum } from '@enum-ms/contract'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { contractSupplierLogisticsPM } from '@/page-permission/contract'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import pagination from '@crud/Pagination'

const permission = contractSupplierLogisticsPM.payment
const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  },
  visibleValue: {
    type: Boolean,
    default: false
  },
  propertyType: {
    type: [Number, String],
    default: undefined
  }
})

const tableRef = ref()
const { crud, CRUD } = useCRUD(
  {
    title: '付款记录',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.logistics-payment',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.visibleValue,
  (val) => {
    if (val) {
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.branchCompanyId = props.currentRow.branchCompanyId
  crud.query.supplierId = props.currentRow.supplierId
  crud.query.propertyType = supplierPayTypeEnum.TRANSPORT.V
  crud.query.auditStatus = auditTypeEnum.PASS.V
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['applyAmount', decimalPrecision.value.contract]],
    toThousandFields: ['applyAmount']
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
.collection-table{
  ::v-deep(.el-select .el-input__inner){
    padding-left:2px;
    padding-right:5px;
  }
  ::v-deep(.el-input-number .el-input__inner, .el-input__inner) {
    text-align: left;
    padding:0 5px;
  }
  ::v-deep(.el-table .cell){
    padding-left:2px;
    padding-right:2px;
  }
}

::v-deep(.pass-tag){
  padding:0 50px;
}
</style>
