<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headerRef" />
    <!-- 订单列表 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60">
        <template #default="{ row, $index }">
           <table-cell-tag :show="row.boolSettlementStatus===!!settlementStatusEnum.SETTLED.V" name="已结算" color="#f56c6c"/>
          <span>{{ $index + 1 }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('serialNumber')" show-overflow-tooltip key="serialNumber" prop="serialNumber" label="订单编号" align="center"/>
      <el-table-column v-if="columns.visible('signDate')" show-overflow-tooltip key="signDate" prop="signDate" label="签订日期" align="center" width="100" />
      <el-table-column v-if="columns.visible('project')" show-overflow-tooltip key="project" prop="project" label="所属项目" min-width="130" />
      <el-table-column v-if="columns.visible('supplierName')" show-overflow-tooltip key="supplierName" prop="supplierName" label="分包单位" min-width="110"/>
      <el-table-column v-if="columns.visible('amount')" prop="amount" key="amount" label="合同额" align="right" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('settlementAmount')" prop="settlementAmount" key="settlementAmount" label="结算额" align="right"  show-overflow-tooltip />
      <el-table-column v-if="columns.visible('subcontractClassName')" show-overflow-tooltip key="subcontractClassName" prop="subcontractClassName" label="分类" />
      <el-table-column v-if="columns.visible('paymentAmount')" prop="paymentAmount" key="paymentAmount" label="付款额" align="right" show-overflow-tooltip>
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
        <template #default="{ row }">
          <div type="warning" class="clickable" @click.stop="openRecord(row, 'payment')" v-if="checkPermission(permission.paymentManage.get)">{{ row.paymentAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('paymentRate')" key="paymentRate" prop="paymentRate" label="付款比例" align="center" width="80">
        <template #default="{ row }">
          <span>{{ row.paymentRate }}%</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceAmount')" prop="invoiceAmount" key="invoiceAmount" label="收票额" align="right" min-width="120" show-overflow-tooltip>
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
        <template #default="{ row }">
          <div class="clickable" @click.stop="openRecord(row, 'invoice')" v-if="checkPermission(permission.invoice)">{{ row.invoiceAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="收票比例" align="center" width="80">
        <template #default="{ row }">
          <span>{{ row.invoiceRate }}%</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.settle,...permission.paymentManage.get])"
        label="操作"
        width="120px"
        align="center"
      >
        <template #default="{ row }">
          <common-button type="primary" icon="el-icon-plus" size="mini" @click="openApplication(row)" v-if="checkPermission(permission.paymentManage.get)" />
          <common-button type="success" icon="el-icon-money" size="mini" @click="openSettle(row)" v-if="(row.boolSettlementStatus!== (!!settlementStatusEnum.SETTLED.V) && !row.unCheckSettlementCount) && checkPermission(permission.settle)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 记录 -->
    <component :is="currentView" v-model="recordVisible" :permission="permission" :detail-info="detailInfo" />
     <!-- 付款申请记录 -->
    <common-drawer
      ref="drawerRef"
      :show-close="true"
      size="80%"
      title="付款申请记录"
      append-to-body
      v-model="applicationVisible"
      :close-on-click-modal="false"
    >
      <template #content>
        <paymentApplication :visibleValue="applicationVisible" :detail-info="detailInfo" />
      </template>
    </common-drawer>
    <settleForm v-model="settleVisible" :detail-info="detailInfo" @success="crud.toQuery" :showType="showType" />
  </div>
</template>

<script setup>
import crudApi from '@/api/project-manage/subcontract-payment/payment-list'
import { ref, provide, computed, nextTick } from 'vue'

import { subcontractOrderPaymentPM as permission } from '@/page-permission/project'
import checkPermission from '@/utils/system/check-permission'
import { settlementStatusEnum } from '@enum-ms/finance'
import { isNotBlank } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import invoiceRecord from './module/invoice-record'
import paymentRecord from './module/payment-record'
import paymentApplication from './module/payment-application'
import settleForm from './module/settle-form'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const currentView = computed(() => {
  if (recordType.value === 'invoice') {
    return invoiceRecord
  }
  return paymentRecord
})

const tableRef = ref()
const headerRef = ref()
const applicationVisible = ref(false)
const settleVisible = ref(false)
const showType = ref('add')

const dataFormat = ref([
  ['signDate', ['parse-time', '{y}-{m}-{d}']],
  ['project', 'parse-project'],
  ['paymentRate', ['to-fixed', 2]],
  ['invoiceRate', ['to-fixed', 2]],
  ['amount', ['to-thousand', decimalPrecision.project]],
  ['paymentAmount', ['to-thousand', decimalPrecision.project]],
  ['invoiceAmount', ['to-thousand', decimalPrecision.project]]
])

const { crud, columns, CRUD } = useCRUD(
  {
    title: '付款申请',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    invisibleColumns: [],
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const detailInfo = ref({})
const orderId = ref(undefined)
const recordType = ref('')
const recordVisible = ref(false)

provide('orderId', orderId)

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    // 付款比例
    v.paymentRate = v.amount ? (v.paymentAmount || 0) / (v.amount || 0) * 100 : 0
    // 收票比例
    v.invoiceRate = v.amount ? (v.invoiceAmount || 0) / (v.amount || 0) * 100 : 0
  })
}

// 打开记录
function openRecord(row, type) {
  if (!checkPermission(permission.detail)) return
  detailInfo.value = row
  recordType.value = type
  orderId.value = row.id
  nextTick(() => {
    recordVisible.value = true
  })
}

function openApplication(row) {
  detailInfo.value = row
  orderId.value = row.id
  nextTick(() => {
    applicationVisible.value = true
  })
}

function openSettle(row) {
  showType.value = isNotBlank(row.settlementInfo) ? 'edit' : 'add'
  detailInfo.value = row
  orderId.value = row.id
  nextTick(() => {
    settleVisible.value = true
  })
}
</script>
<style lang="scss" scoped>
.clickable {
  cursor: pointer;
}
</style>
