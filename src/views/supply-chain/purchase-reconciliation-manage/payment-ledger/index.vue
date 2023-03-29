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
      <el-table-column prop="supplierName" label="供应商" align="center" show-overflow-tooltip min-width="140" />
      <el-table-column prop="typeText" label="类型" align="center" show-overflow-tooltip min-width="120" />
      <el-table-column prop="date" label="筛选日期" align="center" show-overflow-tooltip width="180">
        <template #default>
          <span>{{parseTime(crud.query.startDate,'{y}-{m}-{d}')}}~{{parseTime(crud.query.endDate,'{y}-{m}-{d}')}}</span>
        </template>
      </el-table-column>
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
    <component :is="currentView" v-model="recordVisible" :permission="permission" :detail-info="detailInfo" :query-date="{startDate:crud.query.startDate,endDate:crud.query.endDate}" />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/purchase-reconciliation-manage/jd-payment-ledger'
import { ref, provide, computed, nextTick } from 'vue'

import { parseTime } from '@/utils/date'
import { supplierMaterialPaymentPM as permission } from '@/page-permission/supply-chain'
import checkPermission from '@/utils/system/check-permission'
import { matClsEnum } from '@/utils/enum/modules/classification'
import EO from '@enum'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import inboundRecord from './module/inbound-record'
import invoiceRecord from './module/invoice-record'
import paymentRecord from './module/payment-record'
// import tableCellTag from '@comp-common/table-cell-tag/index.vue'

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

const dataFormat = ref([
  ['createTime', 'parse-time'],
  ['paymentRate', ['to-fixed', 2]],
  ['invoiceRate', ['to-fixed', 2]],
  ['amount', 'to-thousand'],
  ['inboundAmount', 'to-thousand'],
  ['paymentAmount', 'to-thousand'],
  ['invoiceAmount', 'to-thousand']
])

const { crud, CRUD } = useCRUD(
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
    const basicClassArr = EO.getBits(matClsEnum.ENUM, v.supplierClassification, 'L')
    v.typeText = basicClassArr.join(' | ')
    // 付款比例
    v.paymentRate = v.inboundAmount ? (v.paymentAmount || 0) / (v.amount || 0) * 100 : 0
    // 收票比例
    v.invoiceRate = v.inboundAmount ? (v.invoiceAmount || 0) / (v.amount || 0) * 100 : 0
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
