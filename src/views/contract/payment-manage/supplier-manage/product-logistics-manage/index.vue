<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headerRef" />
    <!-- 汇总列表 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
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
      <el-table-column v-if="columns.visible('totalPrice')" prop="totalPrice" key="totalPrice" label="运输费" align="right" min-width="120" show-overflow-tooltip />
      <el-table-column prop="paymentAmount" key="paymentAmount" label="累计已付款" align="right" min-width="120" show-overflow-tooltip>
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
          <div type="warning" class="clickable" @click.stop="openRecord(row, 'payment')">{{ row.paymentAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column prop="paymentRate" label="付款比例" align="center" show-overflow-tooltip min-width="80">
        <template #default="{ row }">
          <span>{{ row.paymentRate }}%</span>
        </template>
      </el-table-column>
      <el-table-column prop="invoiceAmount" key="invoiceAmount" label="累计已收票" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>累计已收票 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div type="warning" class="clickable" @click.stop="openRecord(row, 'invoice')">{{ row.invoiceAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column prop="invoiceRate" label="收票比例" align="center" show-overflow-tooltip min-width="80">
        <template #default="{ row }">
          <span>{{ row.invoiceRate }}%</span>
        </template>
      </el-table-column>
      <!--付款和收票-->
      <el-table-column
        label="操作"
        width="120px"
        align="center"
      >
        <template #default="{ row }">
          <span style="margin-right:5px;display:inline-block;position:relative;padding:10px 0;">
            <common-button type="success" icon="el-icon-money" size="mini" @click="openApplication(row)" />
            <el-badge :value="row.sourceRow.unCheckPaymentCount" :max="99" :hidden="row.sourceRow.unCheckPaymentCount < 1" style="position:absolute;top:-2px;right:-5px;">
            </el-badge>
          </span>
          <span style="margin-right:5px;display:inline-block;position:relative;padding:10px 0;">
            <common-button type="primary" icon="el-icon-tickets" size="mini" @click="openInvoice(row)" />
            <el-badge :value="row.sourceRow.unCheckInvoiceCount" :max="99" :hidden="row.sourceRow.unCheckInvoiceCount < 1" style="position:absolute;top:-2px;right:-5px;">
            </el-badge>
          </span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 记录 -->
    <component :is="currentView" v-model="recordVisible" :permission="permission" :detail-info="detailInfo" :query-date="{startDate:crud.query.startDate,endDate:crud.query.endDate}" />
    <common-drawer
      ref="drawerRef"
      :show-close="true"
      size="85%"
      title="付款申请登记"
      append-to-body
      v-model="applicationVisible"
      :close-on-click-modal="false"
    >
      <template #content>
        <paymentApplication :visibleValue="applicationVisible" :detail-info="detailInfo" @success="crud.toQuery"/>
      </template>
    </common-drawer>
    <common-drawer
      ref="invoiceRef"
      :show-close="true"
      size="85%"
      title="收票申请登记"
      append-to-body
      v-model="invoiceVisible"
      :close-on-click-modal="false"
    >
      <template #content>
        <invoice :visibleValue="invoiceVisible" :detail-info="detailInfo" @success="crud.toQuery"/>
      </template>
    </common-drawer>
    <!-- 物流记录 -->
    <logisticsRecord v-model="logisticsVisible" :permission="permission" :detail-info="detailInfo" />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/jd-product-logistics-manage'
import { ref, provide, computed, nextTick } from 'vue'

import { contractSupplierLogisticsPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import inboundRecord from './module/inbound-record'
import invoiceRecord from './module/invoice-record'
import paymentRecord from './module/payment-record'
import paymentApplication from './module/payment-application/index'
import logisticsRecord from './module/logistics-record'
import invoice from './module/invoice/index'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const currentView = computed(() => {
  if (recordType.value === 'inbound') {
    return inboundRecord
  } else if (recordType.value === 'invoice') {
    return invoiceRecord
  }
  return paymentRecord
})

const tableRef = ref()
const headerRef = ref()
const applicationVisible = ref(false)
const invoiceVisible = ref(false)

const dataFormat = ref([
  ['project', 'parse-project'],
  ['createTime', 'parse-time'],
  ['paymentRate', ['to-fixed', 2]],
  ['invoiceRate', ['to-fixed', 2]],
  ['totalPrice', 'to-thousand'],
  ['amount', 'to-thousand'],
  ['inboundAmount', 'to-thousand'],
  ['paymentAmount', 'to-thousand'],
  ['invoiceAmount', 'to-thousand']
])

const { crud, columns, CRUD } = useCRUD(
  {
    title: '制成品物流',
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
const supplierId = ref(undefined)
const recordType = ref('')
const recordVisible = ref(false)
const logisticsVisible = ref(false)

provide('supplierId', supplierId)

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    v.actualWeight = (v.actualWeight / 1000).toFixed(2)
    // 付款比例
    v.projectId = v.project?.id
    v.paymentRate = v.totalPrice ? (v.paymentAmount || 0) / (v.totalPrice || 0) * 100 : 0
    // 收票比例
    v.invoiceRate = v.totalPrice ? (v.invoiceAmount || 0) / (v.totalPrice || 0) * 100 : 0
  })
}

function openApplication(row) {
  detailInfo.value = row.sourceRow
  supplierId.value = row.supplierId
  applicationVisible.value = true
}

function openInvoice(row) {
  detailInfo.value = row.sourceRow
  supplierId.value = row.supplierId
  invoiceVisible.value = true
}

// 打开记录
function openRecord(row, type) {
  if (!checkPermission(permission.detail)) return
  detailInfo.value = row
  recordType.value = type
  supplierId.value = row.supplierId
  nextTick(() => {
    recordVisible.value = true
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
