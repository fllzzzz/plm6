<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader/>
    </div>
    <!--表格渲染-->
    <common-table
    ref="tableRef"
    v-loading="crud.loading"
    :data="crud.data"
    :empty-text="crud.emptyText"
    :max-height="maxHeight"
    return-source-data
    :showEmptySymbol="false"
    style="width: 100%"
  >
    <el-table-column label="序号" type="index" align="center" width="60">
      <template v-slot="scope">
        <table-cell-tag :show="scope.row.settlementStatus===settlementStatusEnum.SETTLED.V" name="已结算" color="#f56c6c"/>
        <span>{{ scope.$index + 1 }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="采购合同编号" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" :show-overflow-tooltip="true" label="签订日期" align="center" width="80">
      <template v-slot="scope">
        <div>{{ scope.row.createTime? parseTime(scope.row.createTime,'{y}-{m}-{d}'):'-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" label="供应商" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.supplierName? scope.row.supplierName: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('basicClass')" key="basicClass" prop="basicClass" :show-overflow-tooltip="true" label="种类" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.basicClass?EO.getBits(matClsEnum.ENUM, scope.row.basicClass, 'L').join('|'):'' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('amount')" key="amount" prop="amount" label="合同额" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.amount? toThousand(scope.row.amount): '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('settlementAmount')" key="settlementAmount" prop="settlementAmount"  :show-overflow-tooltip="true" label="结算额" align="center">
      <template v-slot="scope">
        <span style="margin-right:10px;" @click="openSettleAudit(scope.row,'detail')">{{ scope.row.settlementAmount? toThousand(scope.row.settlementAmount): '-' }}</span>
        <span @click="openSettleAudit(scope.row,'audit')" style="cursor:pointer;" v-if="checkPermission(crud.permission.payment.get) && scope.row.unCheckSettlementCount>0">
          <el-badge :value="1" :max="99" :hidden="scope.row.unCheckSettlementCount < 1">
            <svg-icon icon-class="notify"  style="color:#e6a23c;font-size:15px;"/>
          </el-badge>
        </span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('inboundAmount')" key="inboundAmount" prop="inboundAmount" label="入库额" align="center">
      <template v-slot="scope">
        <span @click="openStockAmount(scope.row)">{{ scope.row.inboundAmount? toThousand(scope.row.inboundAmount): '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentAmount')" key="paymentAmount" prop="paymentAmount" label="付款额" align="center">
      <template v-slot="scope">
        <span style="cursor:pointer;margin-right:10px;" @click="openTab(scope.row,'payment')">{{ scope.row.paymentAmount? toThousand(scope.row.paymentAmount): '-' }}</span>
        <span @click="openPaymentAudit(scope.row)" style="cursor:pointer;" v-if="checkPermission(crud.permission.payment.get) && scope.row.unCheckPaymentCount>0">
          <el-badge :value="scope.row.unCheckPaymentCount" :max="99" :hidden="scope.row.unCheckPaymentCount < 1">
            <svg-icon icon-class="notify"  style="color:#e6a23c;font-size:15px;"/>
          </el-badge>
        </span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentAmountRate')" key="paymentAmountRate" prop="paymentAmountRate" label="付款比例" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.inboundAmount? ((scope.row.paymentAmount/scope.row.inboundAmount)*100).toFixed(2)+'%': '0.00%' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" label="收票额" align="center">
      <template v-slot="scope">
        <div @click="openTab(scope.row,'invoice')" style="cursor:pointer;">
          <span style="cursor:pointer;margin-right:10px;">{{ scope.row.invoiceAmount? toThousand(scope.row.invoiceAmount): '-' }}</span>
          <template v-if="checkPermission(crud.permission.invoice.get) && scope.row.unCheckInvoiceCount>0">
            <el-badge :value="scope.row.unCheckInvoiceCount" :max="99" :hidden="scope.row.unCheckInvoiceCount < 1">
              <svg-icon icon-class="notify"  style="color:#e6a23c;font-size:15px;"/>
            </el-badge>
          </template>
        </div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="收票比例" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.inboundAmount? ((scope.row.invoiceAmount/scope.row.inboundAmount)*100).toFixed(2)+'%': '0.00%'  }}</div>
      </template>
    </el-table-column>
    <!-- <el-table-column v-if="columns.visible('purchaseStatus')" key="purchaseStatus" prop="purchaseStatus" label="订单状态" align="center" width="80px">
      <template v-slot="scope">
        <el-tag :type="scope.row.purchaseStatus===purchaseOrderStatusEnum.COMPLETE.V?'success':'warning'" effect="plain">{{ isNotBlank(scope.row.purchaseStatus)? purchaseOrderStatusEnum.VL[scope.row.purchaseStatus]:'-' }}</el-tag>
      </template>
    </el-table-column> -->
    <el-table-column v-if="columns.visible('settlementStatus')" key="settlementStatus" prop="settlementStatus" label="结算状态" align="center" width="80px">
      <template v-slot="scope">
        <el-tag v-if="isNotBlank(scope.row.settlementStatus)" :type="scope.row.settlementStatus===settlementStatusEnum.SETTLED.V?'success':'warning'" effect="plain">{{ settlementStatusEnum.VL[scope.row.settlementStatus] }}</el-tag>
        <span v-else>-</span>
      </template>
    </el-table-column>
  </common-table>
  <!-- 入库记录 -->
  <inboundRecord v-model="stockVisible" :detail-info="currentRow" />
  <paymentAudit v-model="auditVisible" :currentRow="currentRow" :propertyType="crud.query.propertyType" @success="crud.toQuery"/>
  <!-- 收付款 -->
  <paymentAndInvoice v-model="tabVisible" :currentRow="currentRow" :tabName="activeName" :propertyType="crud.query.propertyType" @success="crud.toQuery"/>
  <!-- 结算审核 -->
  <settleForm v-model="settleVisible" :detail-info="currentRow" :showType="showType" @success="crud.toQuery"/>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/material-manage'
import { ref } from 'vue'
import { contractSupplierMaterialPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import { settlementStatusEnum } from '@enum-ms/finance'
import inboundRecord from '@/views/supply-chain/purchase-reconciliation-manage/payment-ledger/module/inbound-record'
import paymentAndInvoice from './module/payment-and-invoice'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { isNotBlank } from '@data-type/index'
import { matClsEnum } from '@/utils/enum/modules/classification'
import EO from '@enum'
import paymentAudit from './module/payment-audit/index'
import settleForm from '@/views/supply-chain/purchase-reconciliation-manage/payment-ledger/module/settle-form'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const stockVisible = ref(false)
const tabVisible = ref(false)
const auditVisible = ref(false)
const settleVisible = ref(false)
const currentRow = ref({})
const activeName = ref('payment')
const showType = ref('audit')

const { CRUD, crud, columns } = useCRUD(
  {
    title: '原材料',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
const { maxHeight } = useMaxHeight({
  wrapperBox: '.materialManage',
  paginate: true,
  extraHeight: 40
})

function openStockAmount(row) {
  if (!checkPermission(permission.inbound.get)) {
    return
  }
  currentRow.value = row
  stockVisible.value = true
}

function openTab(row, name) {
  if (!checkPermission(permission[name].get)) {
    return
  }
  activeName.value = name
  currentRow.value = row
  tabVisible.value = true
}

function openPaymentAudit(row) {
  if (!checkPermission(permission.payment.get)) {
    return
  }
  currentRow.value = row
  auditVisible.value = true
}

function openSettleAudit(row, type) {
  if (type === 'detail') {
    if (!row.settlementAmount || !checkPermission(permission.settleDetail)) {
      return
    }
  } else {
    if (!checkPermission(permission.settleAudit)) {
      return
    }
  }
  showType.value = type
  currentRow.value = row
  settleVisible.value = true
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
::v-deep(.el-table .cell){
  padding-left:2px;
  padding-right:2px;
}
::v-deep(.el-tag--small){
  padding:0 3px;
}
::v-deep(.el-badge__content.is-fixed){
  top:5px;
  padding:0 3px;
  line-height:12px;
  height:14px;
}
</style>
