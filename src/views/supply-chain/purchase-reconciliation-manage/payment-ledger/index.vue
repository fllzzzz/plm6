<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headerRef" />
    <!-- 订单列表 -->
    <common-table
      v-if="headerRef?.isOrderType"
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60">
        <template #default="{ row, $index }">
           <table-cell-tag :show="row.settlementStatus===settlementStatusEnum.SETTLED.V" name="已结算" color="#f56c6c"/>
          <span>{{ $index + 1 }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" show-overflow-tooltip key="createTime" prop="createTime" label="日期" align="center" width="130" />
      <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="采购合同编号" align="center" min-width="130" />
      <el-table-column v-if="columns.visible('supplierName')" show-overflow-tooltip key="supplierName" prop="supplierName" label="供应商" min-width="150" />
      <el-table-column v-if="columns.visible('typeText')" show-overflow-tooltip key="typeText" prop="typeText" label="物料种类" min-width="150" />
      <el-table-column v-if="columns.visible('amount')" prop="amount" key="amount" label="合同额" align="right" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('inboundAmount')" prop="inboundAmount" key="inboundAmount" label="入库额" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>入库额 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div type="success" class="clickable" @click.stop="openRecord(row, 'inbound')">{{ row.inboundAmount }}</div>
        </template>
      </el-table-column>
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
          <div type="warning" class="clickable" @click.stop="openRecord(row, 'payment')">{{ row.paymentAmount }}</div>
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
          <div class="clickable" @click.stop="openRecord(row, 'invoice')">{{ row.invoiceAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="收票比例" align="center" width="80">
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
          <common-button type="primary" icon="el-icon-tickets" size="mini" @click="openApplication(row)" v-if="checkPermission(permission.application.get)"/>
          <common-button type="success" icon="el-icon-money" size="mini" @click="openSettle(row)" v-if="(row.settlementStatus!==settlementStatusEnum.SETTLED.V && !row.unCheckSettlementCount) && checkPermission(permission.settle)"/>
        </template>
      </el-table-column>
    </common-table>

    <!-- 汇总列表 -->
    <common-table
      v-if="headerRef && !headerRef?.isOrderType"
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column prop="supplierName" label="供应商" align="center" show-overflow-tooltip min-width="140" />
      <el-table-column prop="amount" label="累计合同额" align="right" show-overflow-tooltip min-width="120" />
      <el-table-column prop="inboundAmount" key="inboundAmount" label="累计入库额" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>累计入库额 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div type="warning" class="clickable" @click.stop="openRecord(row, 'inbound')">{{ row.inboundAmount }}</div>
        </template>
      </el-table-column>
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
        <paymentApplication :visibleValue="applicationVisible" :detail-info="detailInfo"/>
      </template>
    </common-drawer>
    <settleForm v-model="settleVisible" :detail-info="detailInfo" @success="crud.toQuery" :showType="'add'"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { ref, provide, computed, nextTick } from 'vue'

import { supplierMaterialPaymentPM as permission } from '@/page-permission/supply-chain'
import checkPermission from '@/utils/system/check-permission'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { settlementStatusEnum } from '@enum-ms/finance'
import EO from '@enum'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import inboundRecord from './module/inbound-record'
import invoiceRecord from './module/invoice-record'
import paymentRecord from './module/payment-record'
import paymentApplication from './module/payment-application'
import settleForm from './module/settle-form'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'

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
const settleVisible = ref(false)

const dataFormat = ref([
  ['createTime', 'parse-time'],
  ['paymentRate', ['to-fixed', 2]],
  ['invoiceRate', ['to-fixed', 2]],
  ['amount', 'to-thousand'],
  ['inboundAmount', 'to-thousand'],
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
    const basicClassArr = EO.getBits(matClsEnum.ENUM, v.basicClass, 'L')
    v.typeText = basicClassArr.join(' | ')
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
