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
      <el-table-column v-if="columns.visible('supplierName')" key="supplierName" prop="supplierName" :show-overflow-tooltip="true" label="物流公司" align="center" min-width="180">
        <template v-slot="scope">
          <span>{{ scope.row.supplierName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('branchCompanyName')" key="branchCompanyName" prop="branchCompanyName" :show-overflow-tooltip="true" label="付款单位" align="center" min-width="180">
        <template v-slot="scope">
          <span>{{ scope.row.branchCompanyName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('freight')" prop="freight" key="freight" label="物流费" align="right" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('paymentAmount')" prop="paymentAmount" key="paymentAmount" label="付款额" align="right" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('paymentRate')" key="paymentRate" prop="paymentRate" label="付款比例" align="center" width="90">
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
          <div class="clickable" @click.stop="openRecord(row, 'invoice')">{{ row.invoiceAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="收票比例" align="center" width="90">
        <template #default="{ row }">
          <span>{{ row.invoiceRate }}%</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        label="操作"
        width="120px"
        align="center"
      >
        <template #default="{ row }">
          <common-button type="primary" icon="el-icon-plus" size="mini" @click="handleAdd(row)" v-if="checkPermission(permission.add)"/>
          <common-button type="primary" icon="el-icon-tickets" size="mini" @click="openApplication(row)" v-if="checkPermission(permission.application.get)"/>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm :detail-info="detailInfo" />
    <!-- 记录 -->
    <invoiceRecord v-model="recordVisible" :permission="permission" :detail-info="detailInfo"/>
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
        <paymentApplication :visibleValue="applicationVisible" :currentRow="detailInfo" />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { logisticsPaymentList as get } from '@/api/supply-chain/logistics-payment-manage/logistics-record-ledger'
import { add } from '@/api/supply-chain/logistics-payment-manage/logistics-payment'
import { ref, nextTick, computed } from 'vue'
import { supplierLogisticsPaymentPM as permission } from '@/page-permission/supply-chain'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import invoiceRecord from './module/invoice-record'
import paymentApplication from './module/payment-application'
import mForm from './module/form'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const headerRef = ref()
const applicationVisible = ref(false)
const dataFormat = computed(() => {
  return [
    ['paymentRate', ['to-fixed', 2]],
    ['invoiceRate', ['to-fixed', 2]],
    ['freight', ['to-thousand', decimalPrecision.value.supplyChain]],
    ['paymentAmount', ['to-thousand', decimalPrecision.value.supplyChain]],
    ['invoiceAmount', ['to-thousand', decimalPrecision.value.supplyChain]]
  ]
})
const { crud, columns, CRUD } = useCRUD(
  {
    title: '物流付款台账',
    sort: [],
    permission: { ...permission },
    crudApi: { get, add },
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

function handleAdd(row) {
  detailInfo.value = row
  crud.toAdd()
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
  cursor: pointer;
}
</style>
