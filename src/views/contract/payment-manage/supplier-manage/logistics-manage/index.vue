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
    <el-table-column prop="index" label="序号" align="center" width="50" type="index" fixed="left"/>
    <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" :show-overflow-tooltip="true" label="物流单位" align="center" min-width="140">
      <template v-slot="scope">
        <span @click="openStockAmount(scope.row)" style="cursor:pointer;">{{ scope.row.supplierName }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('branchCompanyName')" key="branchCompanyName" prop="branchCompanyName" :show-overflow-tooltip="true" label="签约主体" align="center" min-width="140">
      <template v-slot="scope">
        <span>{{ scope.row.branchCompanyName }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('freight')" key="freight" prop="freight" label="运输额" align="center" min-width="100">
      <template v-slot="scope">
        <span @click="openStockAmount(scope.row)" style="cursor:pointer;">{{ isNotBlank(scope.row.freight)? toThousand(scope.row.freight): 0 }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentAmount')" key="paymentAmount" prop="paymentAmount" label="付款额" align="center" min-width="100">
      <template v-slot="scope">
        <span style="cursor:pointer;margin-right:10px;" @click="openTab(scope.row,'payment')">{{ isNotBlank(scope.row.paymentAmount)? toThousand(scope.row.paymentAmount): 0 }}</span>
        <span @click="openPaymentAudit(scope.row)" style="cursor:pointer;" v-if="checkPermission(crud.permission.get) && scope.row.unCheckPaymentCount>0">
          <el-badge :value="scope.row.unCheckPaymentCount" :max="99" :hidden="scope.row.unCheckPaymentCount < 1">
            <svg-icon icon-class="notify"  style="color:#e6a23c;font-size:15px;"/>
          </el-badge>
        </span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentAmountRate')" key="paymentAmountRate" prop="paymentAmountRate" label="付款比例" align="center" min-width="80">
      <template v-slot="scope">
        <div>{{ scope.row.paymentAmount? ((scope.row.paymentAmount/scope.row.freight)*100).toFixed(2)+'%': 0 }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" label="收票额" align="center" min-width="100">
      <template v-slot="scope">
        <div @click="openTab(scope.row,'invoice')" style="cursor:pointer;">
          <span style="margin-right:10px;">{{ isNotBlank(scope.row.invoiceAmount)? toThousand(scope.row.invoiceAmount): 0 }}</span>
          <template v-if="checkPermission(crud.permission.get) && scope.row.unCheckInvoiceCount>0">
            <el-badge :value="scope.row.unCheckInvoiceCount" :max="99" :hidden="scope.row.unCheckInvoiceCount < 1">
              <svg-icon icon-class="notify"  style="color:#e6a23c;font-size:15px;"/>
            </el-badge>
          </template>
        </div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="收票比例" align="center" min-width="80">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceAmount? ((scope.row.invoiceAmount/scope.row.freight)*100).toFixed(2)+'%': 0  }}</div>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <!-- 物流记录 -->
  <recordDetail v-model="stockVisible" :detailInfo="currentRow" :type="logisticsSearchTypeEnum.COMPANY.V"/>
  <!-- 收付款 -->
  <paymentAndInvoice v-model="tabVisible" :currentRow="currentRow" :tabName="activeName" :propertyType="supplierPayTypeEnum.TRANSPORT.V" @success="crud.toQuery"/>
  <!-- 审核 -->
  <paymentAudit v-model="auditVisible" :currentRow="currentRow" :propertyType="supplierPayTypeEnum.TRANSPORT.V" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import { logisticsPaymentList as get } from '@/api/supply-chain/logistics-payment-manage/logistics-record-ledger'
import { ref } from 'vue'

import { contractSupplierLogisticsPM as permission } from '@/page-permission/contract'
import { logisticsSearchTypeEnum, supplierPayTypeEnum } from '@enum-ms/contract'
import checkPermission from '@/utils/system/check-permission'
import { toThousand } from '@data-type/number'
import { isNotBlank } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import paymentAndInvoice from './module/payment-and-invoice'
import recordDetail from '@/views/supply-chain/logistics-payment-manage/logistics-record/module/record-detail'
import paymentAudit from './module/payment-audit/index'

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
const currentRow = ref({})
const activeName = ref('payment')
const { crud, columns } = useCRUD(
  {
    title: '物流付款',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.logisticsAuditManage',
  paginate: true,
  extraHeight: 40
})

function openStockAmount(row) {
  if (!checkPermission(permission.logisticsLog.get)) {
    return
  }
  currentRow.value = row
  stockVisible.value = true
}

function openTab(row, name) {
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

</script>

<style lang="scss" scoped>
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
