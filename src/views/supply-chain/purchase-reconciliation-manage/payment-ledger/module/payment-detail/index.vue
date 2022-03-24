<template>
  <div class="app-container">
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      :stripe="false"
      return-source-data
      :showEmptySymbol="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="collectionDate" prop="collectionDate" label="*付款日期" align="center" width="160">
        <template v-slot="scope">
          <template v-if="scope.row.type===2">
            <span>合计</span>
          </template>
          <template v-else>
            <div>{{ scope.row.collectionDate? parseTime(scope.row.collectionDate,'{y}-{m}-{d}'): '-' }}</div>
          </template>
        </template>
      </el-table-column>
      <el-table-column key="collectionAmount" prop="collectionAmount" label="*付款金额" align="center" min-width="170" class="money-column">
        <el-table-column key="collectionAmount" prop="collectionAmount" label="金额" align="center" min-width="85">
          <template v-slot="scope">
          <template v-if="scope.row.type===2">
            <span>{{totalAmount?toThousand(totalAmount): totalAmount}}</span>
          </template>
          <template v-else>
            <div>{{ scope.row.collectionAmount && scope.row.collectionAmount>0? toThousand(scope.row.collectionAmount): scope.row.collectionAmount }}</div>
          </template>
        </template>
        </el-table-column>
        <el-table-column key="collectionAmount1" prop="collectionAmount" label="大写" align="center" min-width="85" :show-overflow-tooltip="true">
          <template v-slot="scope">
          <template v-if="scope.row.type===2">
            <span>{{totalAmount?'('+digitUppercase(totalAmount)+')':''}}</span>
          </template>
          <template v-else>
            <div>{{scope.row.collectionAmount?'('+digitUppercase(scope.row.collectionAmount)+')':''}}</div>
          </template>
        </template>
        </el-table-column>
      </el-table-column>
      <el-table-column key="collectionReason" prop="collectionReason" label="*付款事由" align="center" width="120">
        <template v-slot="scope">
         <div>{{ scope.row.collectionReason && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ scope.row.collectionReason]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="collectionMode" prop="collectionMode" label="*付款方式" align="center" width="110">
        <template v-slot="scope">
          <div>{{ scope.row.collectionMode? paymentFineModeEnum.VL[scope.row.collectionMode]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="collectionUnit" prop="collectionUnit" label="*收款单位" align="center" min-width="120" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div>{{ scope.row.collectionUnit }}</div>
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
      <el-table-column key="auditorName" prop="auditorName" label="状态" align="center" width="100px">
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
import { paymentRecord as get } from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { ref, defineProps, watch, inject, nextTick } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import useDict from '@compos/store/use-dict'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { digitUppercase } from '@/utils/data-type/number'
import { contractLedgerPM } from '@/page-permission/contract'

const permission = contractLedgerPM.collection

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const props = defineProps({
  visibleValue: {
    type: Boolean,
    default: false
  }
})

const tableRef = ref()
const dict = useDict(['payment_reason'])
const totalAmount = ref(0)
const orderId = inject('orderId')
const { crud, CRUD } = useCRUD(
  {
    title: '付款填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['orderId', 'propertyType'],
    crudApi: { get },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.purchase-payment',
  paginate: true,
  extraHeight: 40
})

watch(
  orderId,
  (id) => {
    nextTick(() => {
      crud.query.orderId = id
      crud.query.propertyType = 1
      crud.refresh()
    })
  },
  { immediate: true }
)

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.projectId = v.project.id
    return v
  })
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = props.projectId
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  totalAmount.value = 0
  data.data.content.map(v => {
    if (v.collectionAmount) {
      totalAmount.value += v.collectionAmount
    }
  })
  if (totalAmount.value > 0) {
    data.data.content.push({
      type: 2
    })
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
