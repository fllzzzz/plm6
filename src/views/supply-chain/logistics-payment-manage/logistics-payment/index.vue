<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headerRef" />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="[{supplierId:1}]"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60"/>
      <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" :show-overflow-tooltip="true" label="物流公司" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.supplierName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('freight')" prop="freight" key="freight" label="物流费" align="right" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('paymentAmount')" prop="paymentAmount" key="paymentAmount" label="付款额" align="right" min-width="120" show-overflow-tooltip>
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
          <el-tag effect="plain" type="warning" class="clickable" @click.stop="openRecord(row, 'payment')">{{ row.paymentAmount }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('paymentRate')" key="paymentRate" prop="paymentRate" label="付款比例" align="center" width="90">
        <template #default="{ row }">
          <span>{{ row.paymentRate }}%</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceAmount')" prop="invoiceAmount" key="invoiceAmount" label="开票额" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>开票额 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <el-tag effect="plain" class="clickable" @click.stop="openRecord(row, 'invoice')">{{ row.invoiceAmount }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="开票比例" align="center" width="90">
        <template #default="{ row }">
          <span>{{ row.invoiceRate }}%</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        label="操作"
        width="190px"
        align="center"
      >
        <template #default="{ row }">
          <common-button type="primary" icon="el-icon-tickets" size="mini" @click="openApplication(row)" v-if="checkPermission(permission.application.get)"/>
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
  </div>
</template>

<script setup>
import { logisticsPaymentList as get } from '@/api/supply-chain/logistics-payment-manage/logistics-record-ledger'
import { ref, computed, nextTick } from 'vue'
import { supplierLogisticsPaymentPM as permission } from '@/page-permission/supply-chain'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import invoiceRecord from './module/invoice-record'
import paymentRecord from './module/payment-record'
import paymentApplication from './module/payment-application'

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
const dataFormat = ref([
  ['paymentRate', ['to-fixed', 2]],
  ['invoiceRate', ['to-fixed', 2]],
  ['freight', 'to-thousand'],
  ['paymentAmount', 'to-thousand'],
  ['invoiceAmount', 'to-thousand']
])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '物流付款台账',
    sort: [],
    permission: { ...permission },
    crudApi: { get },
    invisibleColumns: [],
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const detailInfo = ref({})
const recordType = ref('')
const recordVisible = ref(false)

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    // 付款比例
    v.paymentRate = v.freight ? (v.paymentAmount || 0) / (v.freight || 0) * 100 : 0
    // 开票比例
    v.invoiceRate = v.freight ? (v.invoiceAmount || 0) / (v.freight || 0) * 100 : 0
  })
}

// 打开记录
function openRecord(row, type) {
  if (!checkPermission(permission.detail)) return
  detailInfo.value = row.sourceRow
  recordType.value = type
  nextTick(() => {
    recordVisible.value = true
  })
}

function openApplication(row) {
  detailInfo.value = row.sourceRow
  nextTick(() => {
    applicationVisible.value = true
  })
}
</script>
<style lang="scss" scoped>
.clickable {
  width: 100%;
  cursor: pointer;
}
</style>