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
      <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" :show-overflow-tooltip="true" label="物流公司" align="center" min-width="180">
        <template v-slot="scope">
          <span>{{ scope.row.supplierName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('trainNumber')" key="trainNumber" prop="trainNumber" :show-overflow-tooltip="true" label="车次" align="center" />
      <el-table-column v-if="columns.visible('loadingWeight')" key="loadingWeight" prop="loadingWeight" :show-overflow-tooltip="true" label="过磅重量(吨)" align="center" min-width="180">
        <template v-slot="scope">
          <span>{{ scope.row.loadingWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('other')" key="other" prop="other" :show-overflow-tooltip="true" label="关联入库及供方" align="center" min-width="180">
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="info" size="mini" @click="openRecord(scope.row, 'logistics')"/>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('freight')" prop="freight" key="freight" label="运输费" align="right" min-width="120" show-overflow-tooltip />
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
      <el-table-column prop="invoiceAble" label="应收发票" align="right" show-overflow-tooltip min-width="120">
        <template v-slot="scope">
          <span :style="`color:${(scope.row.sourceRow.freight-scope.row.sourceRow.invoiceAmount)<0?'red':''}`">{{toThousand(scope.row.sourceRow.freight-scope.row.sourceRow.invoiceAmount)}}</span>
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
      <el-table-column prop="payable" label="应付运费" align="right" show-overflow-tooltip min-width="120">
        <template v-slot="scope">
          <span :style="`color:${(scope.row.sourceRow.freight-scope.row.sourceRow.paymentAmount)<0?'red':''}`">{{toThousand(scope.row.sourceRow.freight-scope.row.sourceRow.paymentAmount)}}</span>
        </template>
      </el-table-column>
      <!--付款和收票-->
      <el-table-column
        v-if="checkPermission([...permission.payment.get,...permission.invoice.get])"
        label="操作"
        width="120px"
        align="center"
      >
        <template #default="{ row }">
          <span style="margin-right:5px;display:inline-block;position:relative;padding:10px 0;" v-if="checkPermission(permission.payment.get)">
            <common-button type="success" icon="el-icon-money" size="mini" @click="openApplication(row)" />
            <el-badge :value="row.sourceRow.unCheckPaymentCount" :max="99" :hidden="row.sourceRow.unCheckPaymentCount < 1" style="position:absolute;top:1px;right:-5px;">
            </el-badge>
          </span>
          <span style="margin-right:5px;display:inline-block;position:relative;padding:10px 0;" v-if="checkPermission(permission.invoice.get)">
            <common-button type="primary" icon="el-icon-tickets" size="mini" @click="openInvoice(row)" />
            <el-badge :value="row.sourceRow.unCheckInvoiceCount" :max="99" :hidden="row.sourceRow.unCheckInvoiceCount < 1" style="position:absolute;top:1px;right:-5px;">
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
      <template #titleAfter>
        <span>物流公司：{{detailInfo.supplierName}}</span>
      </template>
      <template #content>
        <paymentApplication :visibleValue="applicationVisible" :detail-info="detailInfo" @success="crud.toQuery"/>
      </template>
    </common-drawer>
    <common-drawer
      ref="invoiceRef"
      :show-close="true"
      size="100%"
      title="收票申请登记"
      append-to-body
      v-model="invoiceVisible"
      :close-on-click-modal="false"
    >
      <template #titleAfter>
        <span>物流公司：{{detailInfo.supplierName}}</span>
      </template>
      <template #content>
        <invoice :visibleValue="invoiceVisible" :detail-info="detailInfo" @success="crud.toQuery"/>
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/jd-logistics-manage'
import { ref, provide, computed, nextTick } from 'vue'

import { contractSupplierLogisticsPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'
import { toThousand } from '@data-type/number'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import logisticsRecord from './module/logistics-record'
import invoiceRecord from './module/invoice-record'
import paymentRecord from './module/payment-record'
import paymentApplication from './module/payment-application/index'
import invoice from './module/invoice/index'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const currentView = computed(() => {
  if (recordType.value === 'logistics') {
    return logisticsRecord
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
  ['createTime', 'parse-time'],
  ['paymentRate', ['to-fixed', 2]],
  ['invoiceRate', ['to-fixed', 2]],
  ['loadingWeight', ['to-fixed', 2]],
  ['amount', 'to-thousand'],
  ['inboundAmount', 'to-thousand'],
  ['paymentAmount', 'to-thousand'],
  ['invoiceAmount', 'to-thousand']
])

const { crud, columns, CRUD } = useCRUD(
  {
    title: '原材料物流',
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

provide('supplierId', supplierId)

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    // 付款比例
    v.loadingWeight = v.loadingWeight ? v.loadingWeight / 1000 : 0
    v.paymentRate = v.freight ? (v.paymentAmount || 0) / (v.freight || 0) * 100 : 0
    // 收票比例
    v.invoiceRate = v.freight ? (v.invoiceAmount || 0) / (v.freight || 0) * 100 : 0
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
</script>
<style lang="scss" scoped>
.clickable {
  cursor: pointer;
  color:#409eff;
}
</style>
