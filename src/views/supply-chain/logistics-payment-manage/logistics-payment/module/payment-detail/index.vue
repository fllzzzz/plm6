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
      <el-table-column key="paymentMethod" prop="paymentMethod" label="*付款方式" align="center" width="110">
        <template v-slot="scope">
          <div>{{ scope.row.paymentMethod? paymentFineModeEnum.VL[scope.row.paymentMethod]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentBankAccount" prop="paymentBankAccount" :show-overflow-tooltip="true" label="*付款银行" align="center" min-width="120">
        <template v-slot="scope">
          <div>{{ scope.row.paymentBankAccount?scope.row.paymentBankAccount:'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="receivingUnit" prop="receivingUnit" label="*收款单位" align="center" min-width="120" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div>{{ scope.row.receivingUnit?scope.row.receivingUnit:'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="applyUserName" prop="applyUserName" label="办理人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.applyUserName?scope.row.applyUserName:'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditUserName" prop="auditUserName" label="审核人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.auditUserName?scope.row.auditUserName:'-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditStatus" prop="auditStatus" label="审核状态" align="center">
        <template v-slot="scope">
          <el-tag type="warning" v-if="scope.row.auditStatus===auditTypeEnum.REJECT.V">{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
          <el-tag :type="scope.row.auditStatus===auditTypeEnum.PASS.V?'success':''" v-else>{{ auditTypeEnum.VL[scope.row.auditStatus] }}</el-tag>
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
import { auditTypeEnum } from '@enum-ms/contract'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { digitUppercase } from '@/utils/data-type/number'
import { supplierLogisticsPaymentPM } from '@/page-permission/supply-chain'

const permission = supplierLogisticsPaymentPM.paymentLog

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
  },
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const tableRef = ref()
const orderId = inject('orderId')
const { crud } = useCRUD(
  {
    title: '付款记录',
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
      crud.query.propertyType = props.detailInfo.propertyType
      crud.refresh()
    })
  },
  { immediate: true }
)

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
