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
    :data-format="dataFormat"
    :empty-text="crud.emptyText"
    :max-height="maxHeight"
    style="width: 100%"
  >
    <el-table-column label="序号" type="index" align="center" width="60">
        <template #default="{ row, $index }">
           <table-cell-tag :show="row.boolSettlementStatus===!!settlementStatusEnum.SETTLED.V" name="已结算" color="#f56c6c"/>
          <span>{{ $index + 1 }}</span>
        </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('serialNumber')" show-overflow-tooltip key="serialNumber" prop="serialNumber" label="分包订单编号" align="center"/>
    <el-table-column v-if="columns.visible('signDate')" show-overflow-tooltip key="signDate" prop="signDate" label="签订日期" align="center" width="100" />
    <el-table-column v-if="columns.visible('project')" show-overflow-tooltip key="project" prop="project" label="所属项目" min-width="130" />
    <el-table-column v-if="columns.visible('supplierName')" show-overflow-tooltip key="supplierName" prop="supplierName" label="分包单位" min-width="110"/>
    <el-table-column v-if="columns.visible('amount')" prop="amount" key="amount" label="合同额" align="right" show-overflow-tooltip />
    <el-table-column v-if="columns.visible('settlementAmount')" key="settlementAmount" prop="settlementAmount"  :show-overflow-tooltip="true" label="结算额" align="center">
      <template v-slot="scope">
        <span style="margin-right:10px;" @click="openSettleAudit(scope.row,'detail')">{{ scope.row.sourceRow.settlementAmount? toThousand(scope.row.sourceRow.settlementAmount,decimalPrecision.contract): '-' }}</span>
        <span @click="openSettleAudit(scope.row,'audit')" style="cursor:pointer;" v-if="checkPermission(permission.settleAudit) && scope.row.sourceRow.unCheckSettlementCount>0">
          <el-badge :value="1" :max="99" :hidden="scope.row.sourceRow.unCheckSettlementCount < 1">
            <svg-icon icon-class="notify"  style="color:#e6a23c;font-size:15px;"/>
          </el-badge>
        </span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('subcontractClassName')" show-overflow-tooltip key="subcontractClassName" prop="subcontractClassName" label="分类" />
    <el-table-column v-if="columns.visible('paymentAmount')" prop="paymentAmount" key="paymentAmount" label="付款额" align="center" show-overflow-tooltip>
      <template v-if="checkPermission(permission.detail)" #header>
        <el-tooltip
          effect="light"
          placement="top"
          content="点击行可以查看详情"
        >
          <div style="display: inline-block">
            <span>付款额 </span>
            <i class="el-icon-info" />
          </div>
        </el-tooltip>
      </template>
      <template v-slot="scope">
        <span style="cursor:pointer;margin-right:10px;" @click="openTab(scope.row,'payment')">{{ scope.row.sourceRow.paymentAmount? toThousand(scope.row.sourceRow.paymentAmount,decimalPrecision.contract): '-' }}</span>
        <span @click="openPaymentAudit(scope.row)" style="cursor:pointer;" v-if="checkPermission(crud.permission.payment.audit) && scope.row.unCheckPaymentCount>0">
          <el-badge :value="scope.row.sourceRow.unCheckPaymentCount" :max="99" :hidden="scope.row.sourceRow.unCheckPaymentCount < 1">
            <svg-icon icon-class="notify"  style="color:#e6a23c;font-size:15px;"/>
          </el-badge>
        </span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('paymentRate')" key="paymentRate" prop="paymentRate" label="付款比例" align="center" width="80">
      <template v-slot="scope">
        <span>{{ scope.row.paymentRate }}%</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceAmount')" prop="invoiceAmount" key="invoiceAmount" label="收票额" align="center" min-width="120" show-overflow-tooltip>
      <template v-if="checkPermission(permission.detail)" #header>
        <el-tooltip
          effect="light"
          placement="top"
          content="点击行可以查看详情"
        >
          <div style="display: inline-block">
            <span>收票额 </span>
            <i class="el-icon-info" />
          </div>
        </el-tooltip>
      </template>
      <template v-slot="scope">
        <div @click="openTab(scope.row,'invoice')" style="cursor:pointer;">
          <span style="cursor:pointer;margin-right:10px;">{{ scope.row.sourceRow.invoiceAmount? toThousand(scope.row.sourceRow.invoiceAmount,decimalPrecision.contract): '-' }}</span>
          <template v-if="checkPermission(crud.permission.invoice.audit) && scope.row.sourceRow.unCheckInvoiceCount>0">
            <el-badge :value="scope.row.sourceRow.unCheckInvoiceCount" :max="99" :hidden="scope.row.sourceRow.unCheckInvoiceCount < 1">
              <svg-icon icon-class="notify"  style="color:#e6a23c;font-size:15px;"/>
            </el-badge>
          </template>
        </div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="收票比例" align="center" width="80">
      <template v-slot="scope">
        <span>{{ scope.row.invoiceRate }}%</span>
      </template>
    </el-table-column>
  </common-table>
  <paymentAudit v-model="auditVisible" :currentRow="currentRow" :propertyType="crud.query.propertyType" @success="crud.toQuery"/>
  <!-- 收付款 -->
  <paymentAndInvoice v-model="tabVisible" :currentRow="currentRow" :tabName="activeName" :propertyType="crud.query.propertyType" @success="crud.toQuery" :permission="permission"/>
  <!-- 结算审核 -->
  <settleForm v-model="settleVisible" :detail-info="currentRow" :showType="showType" @success="crud.toQuery"/>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/project-manage/subcontract-payment/payment-list'
import { ref, computed } from 'vue'

import { contractSupplierSubcontractPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { settlementStatusEnum } from '@enum-ms/finance'
import { toThousand } from '@data-type/number'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import pagination from '@crud/Pagination'
import mHeader from './module/header'
import paymentAndInvoice from './module/payment-and-invoice'
import paymentAudit from './module/payment-audit/index'
import settleForm from '@/views/project-manage/subcontract-manage/subcontract-payment/module/settle-form'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const tabVisible = ref(false)
const auditVisible = ref(false)
const settleVisible = ref(false)
const currentRow = ref({})
const activeName = ref('payment')
const showType = ref('audit')

const dataFormat = computed(() => {
  return [
    ['signDate', ['parse-time', '{y}-{m}-{d}']],
    ['project', 'parse-project'],
    ['paymentRate', ['to-fixed', 2]],
    ['invoiceRate', ['to-fixed', 2]],
    ['amount', ['to-thousand', decimalPrecision.value.contract]],
    ['paymentAmount', ['to-thousand', decimalPrecision.value.contract]],
    ['invoiceAmount', ['to-thousand', decimalPrecision.value.contract]]
  ]
})

const { CRUD, crud, columns } = useCRUD(
  {
    title: '分包订单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
const { maxHeight } = useMaxHeight({
  wrapperBox: '.subcontractOrderManage',
  paginate: true,
  extraHeight: 40
})

function openTab(row, name) {
  if (!checkPermission(permission[name].get)) {
    return
  }
  activeName.value = name
  currentRow.value = row.sourceRow
  tabVisible.value = true
}

function openPaymentAudit(row) {
  if (!checkPermission(permission.payment.get)) {
    return
  }
  currentRow.value = row.sourceRow
  auditVisible.value = true
}

function openSettleAudit(row, type) {
  if (type === 'detail') {
    if (!row.sourceRow.settlementAmount || !checkPermission(permission.settleDetail)) {
      return
    }
  } else {
    if (!checkPermission(permission.settleAudit)) {
      return
    }
  }
  showType.value = type
  row.sourceRow.unCheckSettlementAmount = row.settlementInfo?.amount
  row.sourceRow.attachmentDTOS = row.settlementInfo?.attachments
  row.sourceRow.settleNotBoolDeduct = !row.settlementInfo?.boolDeduct
  currentRow.value = row.sourceRow
  settleVisible.value = true
}

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    // 付款比例
    v.paymentRate = v.amount ? (v.paymentAmount || 0) / (v.amount || 0) * 100 : 0
    // 收票比例
    v.invoiceRate = v.amount ? (v.invoiceAmount || 0) / (v.amount || 0) * 100 : 0
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
