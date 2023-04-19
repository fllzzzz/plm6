<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headerRef" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60"/>
      <el-table-column v-if="columns.visible('project')" key="project" prop="project" :show-overflow-tooltip="true" label="项目" align="center" min-width="180" />
      <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" :show-overflow-tooltip="true" label="物流公司" align="center" min-width="180">
        <template v-slot="scope">
          <span>{{ scope.row.supplierName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('trainNumber')" key="trainNumber" prop="trainNumber" :show-overflow-tooltip="true" label="车次" align="center">
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>车次 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div class="clickable" @click.stop="openLogisticsRecord(row)">{{ row.trainNumber }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('actualWeight')" key="actualWeight" prop="actualWeight" :show-overflow-tooltip="true" label="累计发运量(吨)" align="center" min-width="180">
        <template v-slot="scope">
          <span>{{ scope.row.actualWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('totalPrice')" prop="totalPrice" key="totalPrice" label="运输费" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>运输费 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div class="clickable" @click.stop="openLogisticsRecord(row)">{{ row.totalPrice }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('paymentAmount')" prop="paymentAmount" key="paymentAmount" label="累计付款" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>累计已付款 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div class="clickable" @click.stop="openPaymentRecord(row)">{{ row.paymentAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('paymentRate')" key="paymentRate" prop="paymentRate" label="付款比例" align="center" width="90">
        <template #default="{ row }">
          <span>{{ row.paymentRate }}%</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceAmount')" prop="invoiceAmount" key="invoiceAmount" label="累计收票" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>累计收票</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div class="clickable" @click.stop="openRecord(row)">{{ row.invoiceAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="收票比例" align="center" width="90">
        <template #default="{ row }">
          <span>{{ row.invoiceRate }}%</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 发票记录 -->
    <invoiceRecord v-model="recordVisible" :permission="permission" :detail-info="detailInfo" :query-date="{startDate:crud.query.startDate,endDate:crud.query.endDate}"/>
     <!-- 付款记录 -->
    <paymentRecord v-model="paymentVisible" :permission="permission" :detail-info="detailInfo" :query-date="{startDate:crud.query.startDate,endDate:crud.query.endDate}"/>
    <!-- 物流记录 -->
    <logisticsRecord v-model="logisticsVisible" :permission="permission" :detail-info="detailInfo" />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/logistics-payment-manage/jd-product-logistics-record-ledger'
import { ref, nextTick } from 'vue'
import { supplierLogisticsPaymentPM as permission } from '@/page-permission/supply-chain'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import invoiceRecord from './module/invoice-record'
import paymentRecord from './module/payment-record'
import logisticsRecord from './module/logistics-record'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const headerRef = ref()
const paymentVisible = ref(false)
const dataFormat = ref([
  ['project', ['parse-project']],
  ['paymentRate', ['to-fixed', 2]],
  ['invoiceRate', ['to-fixed', 2]],
  ['totalPrice', 'to-thousand'],
  ['paymentAmount', 'to-thousand'],
  ['invoiceAmount', 'to-thousand']
])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '制成品物流',
    sort: [],
    permission: { ...permission },
    crudApi,
    invisibleColumns: [],
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const detailInfo = ref({})
const recordVisible = ref(false)
const logisticsVisible = ref(false)

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    // 付款比例
    v.actualWeight = (v.actualWeight / 1000).toFixed(2)
    v.projectId = v.project?.id
    v.paymentRate = v.freight ? (v.paymentAmount || 0) / (v.freight || 0) * 100 : 0
    // 开票比例
    v.invoiceRate = v.freight ? (v.invoiceAmount || 0) / (v.freight || 0) * 100 : 0
  })
}

// 打开发票记录
function openRecord(row) {
  if (!checkPermission(permission.detail)) return
  detailInfo.value = row.sourceRow
  nextTick(() => {
    recordVisible.value = true
  })
}

// 打开付款记录
function openPaymentRecord(row) {
  if (!checkPermission(permission.detail)) return
  detailInfo.value = row.sourceRow
  nextTick(() => {
    paymentVisible.value = true
  })
}

// 打开物流记录
function openLogisticsRecord(row) {
  if (!checkPermission(permission.detail)) return
  detailInfo.value = row.sourceRow
  nextTick(() => {
    logisticsVisible.value = true
  })
}
</script>
<style lang="scss" scoped>
.clickable {
  cursor: pointer;
  color:#409eff;
}
</style>
