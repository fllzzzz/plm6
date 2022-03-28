<template>
  <div class="app-container">
    <!--表格渲染-->
    <div>
      <el-tag type="success" size="medium" v-if="currentRow.amount">{{`合同额:${toThousand(currentRow.amount)}`}}</el-tag>
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
      <el-table-column key="paymentDate" prop="paymentDate" label="*付款日期" align="center" width="160">
        <template v-slot="scope">
          <div>{{ scope.row.paymentDate? parseTime(scope.row.paymentDate,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="applyAmount1" prop="applyAmount1" label="*付款金额" align="center" min-width="170" class="money-column">
        <el-table-column key="applyAmount" prop="applyAmount" label="金额" align="center" min-width="85">
          <template v-slot="scope">
            <div>{{ scope.row.applyAmount && scope.row.applyAmount>0? toThousand(scope.row.applyAmount): scope.row.applyAmount }}</div>
          </template>
        </el-table-column>
        <el-table-column key="applyAmount2" prop="applyAmount2" label="大写" align="center" min-width="85" :show-overflow-tooltip="true">
          <template v-slot="scope">
            <div>{{scope.row.applyAmount?'('+digitUppercase(scope.row.applyAmount)+')':''}}</div>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column key="paymentReasonId" prop="paymentReasonId" label="*付款事由" align="center" width="120">
        <template v-slot="scope">
          <div>{{ scope.row.paymentReasonId && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ scope.row.paymentReasonId]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="collectionMode" prop="collectionMode" label="*付款方式" align="center" width="110">
        <template v-slot="scope">
          <div>{{ scope.row.collectionMode? paymentFineModeEnum.VL[scope.row.collectionMode]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="collectionBankAccountId" prop="collectionBankAccountId" :show-overflow-tooltip="true" label="*付款银行" align="center" min-width="120">
        <template v-slot="scope">
         <div>{{ scope.row.collectionDepositBank }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentUnit" prop="paymentUnit" label="*收款单位" align="center" min-width="120" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div>{{ scope.row.paymentUnit  }}</div>
        </template>
      </el-table-column>
      <el-table-column key="attachment" prop="attachment" label="附件" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.attachment }}</div>
        </template>
      </el-table-column>
      <el-table-column key="writtenByName" prop="writtenByName" label="办理人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.writtenByName }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditorName" prop="auditorName" label="审核人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.auditorName }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditorName" prop="auditorName" label="审核日期" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.auditorName }}</div>
        </template>
      </el-table-column>
    </common-table>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/pay-invoice/pay'
import { ref, defineProps, watch } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import useDict from '@compos/store/use-dict'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { digitUppercase } from '@/utils/data-type/number'
import { contractSupplierMaterialPM } from '@/page-permission/contract'

const permission = contractSupplierMaterialPM.payment

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
const dict = useDict(['payment_reason'])
const { crud, CRUD } = useCRUD(
  {
    title: '付款填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['haveApplyAmount', 'collectionMode', 'collectionReason', 'collectionDepositBank', 'collectionBankAccount', 'paymentBankAccount', 'paymentDepositBank', 'auditorName', 'auditTime'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.material-payment',
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
  crud.query.orderId = props.currentRow.id
  crud.query.propertyType = props.propertyType
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['applyAmount'],
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
