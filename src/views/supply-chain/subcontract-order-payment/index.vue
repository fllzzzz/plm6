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
      <el-table-column label="序号" type="index" align="center" width="60"/>
      <!-- <el-table-column v-if="columns.visible('signDate')" show-overflow-tooltip key="signDate" prop="signDate" label="签订日期" align="center" width="100" /> -->
      <el-table-column v-if="columns.visible('project')" show-overflow-tooltip key="project" prop="project" label="所属项目" min-width="130" />
      <el-table-column v-if="columns.visible('supplierName')" show-overflow-tooltip key="supplierName" prop="supplierName" label="分包单位" min-width="110"/>
      <el-table-column v-if="columns.visible('subcontractClassName')" show-overflow-tooltip key="subcontractClassName" prop="subcontractClassName" label="分包类别" />
      <el-table-column v-if="columns.visible('amount')" prop="amount" key="amount" label="合同额" align="right" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('settlementAmount')" prop="settlementAmount" key="settlementAmount" label="结算额" align="right"  show-overflow-tooltip />
      <el-table-column v-if="columns.visible('paymentAmount')" prop="paymentAmount" key="paymentAmount" label="付款额" align="right" show-overflow-tooltip>
        <template v-if="checkPermission(permission.payment.get)" #header>
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
          <div type="warning" class="clickable" @click.stop="openRecord(row, 'payment')" v-if="checkPermission(permission.payment.get)">{{ row.paymentAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('paymentRate')" key="paymentRate" prop="paymentRate" label="付款比例" align="center" width="80">
        <template #default="{ row }">
          <span>{{ row.paymentRate }}%</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceAmount')" prop="invoiceAmount" key="invoiceAmount" label="收票额" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.invoice.get)" #header>
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
          <div class="clickable" @click.stop="openRecord(row, 'invoice')" v-if="checkPermission(permission.invoice.get)">{{ row.invoiceAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="收票比例" align="center" width="80">
        <template #default="{ row }">
          <span>{{ row.invoiceRate }}%</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 记录 -->
    <component :is="currentView" v-model="recordVisible" :permission="permission" :detail-info="detailInfo" />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/subcontract-manage/jd-subcontract-payment'
import { ref, provide, computed, nextTick } from 'vue'

import { supplyChainSubcontractPaymentPM as permission } from '@/page-permission/supply-chain'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import invoiceRecord from './module/invoice-record'
import paymentRecord from './module/payment-record'

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

const dataFormat = ref([
  ['signDate', ['parse-time', '{y}-{m}-{d}']],
  ['project', 'parse-project'],
  ['paymentRate', ['to-fixed', 2]],
  ['invoiceRate', ['to-fixed', 2]],
  ['amount', 'to-thousand'],
  ['paymentAmount', 'to-thousand'],
  ['invoiceAmount', 'to-thousand']
])

const { crud, columns, CRUD } = useCRUD(
  {
    title: '付款台账',
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
    v.projectId = v.project?.id
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
</script>
<style lang="scss" scoped>
.clickable {
  cursor: pointer;
  color:#409eff;
}
</style>
