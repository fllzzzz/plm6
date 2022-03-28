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
    <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="采购订单" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" :show-overflow-tooltip="true" label="签订日期" align="center" width="80">
      <template v-slot="scope">
        <div>{{ scope.row.createTime? parseTime(scope.row.signingDate,'{y}-{m}-{d}'):'-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" label="分包单位" align="center">
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
        <span>{{ isNotBlank(scope.row.amount)? toThousand(scope.row.amount): 0 }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('settlementAmount')" key="settlementAmount" prop="settlementAmount"  :show-overflow-tooltip="true" label="结算额" align="center">
      <template v-slot="scope">
        <span>{{ isNotBlank(scope.row.settlementAmount)? toThousand(scope.row.settlementAmount): 0 }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('inboundAmount')" key="inboundAmount" prop="inboundAmount" label="入库额" align="center">
      <template v-slot="scope">
        <span @click="openStockAmount(scope.row)">{{ isNotBlank(scope.row.inboundAmount)? toThousand(scope.row.inboundAmount): 0 }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentAmount')" key="paymentAmount" prop="paymentAmount" label="付款额" align="center">
      <template v-slot="scope">
        <span style="cursor:pointer;margin-right:10px;" @click="openTab(scope.row,'payment')">{{ isNotBlank(scope.row.paymentAmount)? toThousand(scope.row.paymentAmount): 0 }}</span>
        <span @click="openPaymentAudit(scope.row)" style="cursor:pointer;" v-if="checkPermission(crud.permission.get) && scope.row.unCheckPaymentCount>0">
          <el-badge :value="scope.row.unCheckPaymentCount" :max="99" :hidden="scope.row.unCheckPaymentCount < 1">
            <svg-icon icon-class="notify"  style="color:#e6a23c;font-size:15px;"/>
          </el-badge>
        </span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentAmountRate')" key="paymentAmountRate" prop="paymentAmountRate" label="付款比例" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.paymentAmount? ((scope.row.paymentAmount/scope.row.amount)*100).toFixed(2)+'%': 0 }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" label="收票额" align="center">
      <template v-slot="scope">
        <div @click="openTab(scope.row,'invoice')">
          <span style="cursor:pointer;margin-right:10px;">{{ isNotBlank(scope.row.invoiceAmount)? toThousand(scope.row.invoiceAmount): 0 }}</span>
          <template v-if="checkPermission(crud.permission.get) && scope.row.unCheckInvoiceAmount < 0">
            <el-badge :value="scope.row.unCheckInvoiceAmount" :max="99" :hidden="scope.row.unCheckInvoiceAmount < 1">
              <svg-icon icon-class="notify"  style="color:#e6a23c;font-size:15px;"/>
            </el-badge>
          </template>
        </div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="收票比例" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.invoiceAmount? ((scope.row.invoiceAmount/scope.row.amount)*100).toFixed(2)+'%': 0  }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('purchaseStatus')" key="purchaseStatus" prop="purchaseStatus" label="订单状态" align="center" width="80px">
      <template v-slot="scope">
        <el-tag :type="scope.row.purchaseStatus===purchaseOrderStatusEnum.COMPLETE.V?'success':'warning'" effect="plain">{{ isNotBlank(scope.row.purchaseStatus)? purchaseOrderStatusEnum.VL[scope.row.purchaseStatus]:'-' }}</el-tag>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('settlementStatus')" key="settlementStatus" prop="settlementStatus" label="结算状态" align="center" width="80px">
      <template v-slot="scope">
        <template v-if="isNotBlank(scope.row.settlementStatus)">
          <el-tag :type="scope.row.settlementStatus===settlementStatusEnum.SETTLED.V?'success':'warning'" effect="plain">{{ settlementStatusEnum.VL[scope.row.settlementStatus] }}</el-tag>
        </template>
      </template>
    </el-table-column>
  </common-table>
  <!-- 发生额 -->
  <stock-amount v-model="stockVisible"/>
  <!-- 收付款 -->
  <paymentAndInvoice v-model="tabVisible" :currentRow="currentRow" :tabName="activeName" :propertyType="crud.query.propertyType" @success="crud.toQuery"/>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/material-manage'
import { ref } from 'vue'
import { contractSupplierProductPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import { settlementStatusEnum, purchaseOrderStatusEnum } from '@enum-ms/contract'
import stockAmount from './module/stock-amount'
import paymentAndInvoice from './module/payment-and-invoice'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'
import { isNotBlank } from '@data-type/index'
import { matClsEnum } from '@/utils/enum/modules/classification'
import EO from '@enum'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const stockVisible = ref(false)
const tabVisible = ref(false)
const currentRow = ref({})
const activeName = ref('payment')
const { CRUD, crud, columns } = useCRUD(
  {
    title: '制成品',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.productManage',
  paginate: true,
  extraHeight: 40
})

function openStockAmount(row) {
  if (!checkPermission(permission.inbound.get)) {
    return
  }
  currentRow.value = row
  // currentProjectId.value = row
  stockVisible.value = true
}

function openTab(row, name) {
  activeName.value = name
  currentRow.value = row
  // currentProjectId.value = row
  tabVisible.value = true
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
