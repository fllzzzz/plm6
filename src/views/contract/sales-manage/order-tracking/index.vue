<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="50" />
      <el-table-column v-if="columns.visible('status')" key="status" prop="status" :show-overflow-tooltip="true" align="center" label="状态" width="80">
        <template #default="{ row }">
          <el-tag :type="projectStatusEnum.V[row?.sourceRow?.status].TAG" size="medium" effect="plain">{{ row.status }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('project')" key="project" prop="project" :show-overflow-tooltip="true" label="项目"  min-width="150" />
      <el-table-column v-if="columns.visible('orderSourceType')" key="orderSourceType" prop="orderSourceType" :show-overflow-tooltip="true" label="订单属性"  width="80" />
      <el-table-column v-if="columns.visible('projectContentName')" key="projectContentName" prop="projectContentName" label="合同内容" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('contractAmount')" prop="contractAmount" key="contractAmount" label="合同额" align="right" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('settlementAmount')" prop="settlementAmount" key="settlementAmount" label="结算额" align="right" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('collectionAmount')" prop="collectionAmount" key="collectionAmount" label="累计收款额" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>累计收款额 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div class="clickable" @click.stop="openRecord(row, 'collection')">{{ row.collectionAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('collectionRate')" key="collectionRate" prop="collectionRate" label="收款比例" align="center" width="90" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ row.collectionRate }}%</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceAmount')" prop="invoiceAmount" key="invoiceAmount" label="累计开票额" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>累计开票额 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div class="clickable" @click.stop="openRecord(row, 'invoice')">{{ row.invoiceAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="开票比例" align="center" width="80" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ row.invoiceRate }}%</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('warehouseAmount')" prop="warehouseAmount" key="warehouseAmount" label="累计入库额" align="right" min-width="120" show-overflow-tooltip>
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
          <div class="clickable" @click.stop="openRecord(row, 'warehouse')">{{ row.warehouseAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('warehouseRate')" key="warehouseRate" prop="warehouseRate" label="入库比例" align="center" width="80" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ row.warehouseRate }}%</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('happenedAmount')" prop="happenedAmount" key="happenedAmount" label="累计出库额" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>累计出库额 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <div class="clickable" @click.stop="openRecord(row, 'happened')">{{ row.happenedAmount }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('happenedRate')" key="happenedRate" prop="happenedRate" label="出库额比例" align="center" width="90" show-overflow-tooltip>
        <template #default="{ row }">
          <span>{{ row.happenedRate }}%</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('transportQuantity')" key="transportQuantity" prop="transportQuantity" label="运输车次" align="center" width="80" show-overflow-tooltip>
        <!-- <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>运输车次 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template> -->
        <template #default="{ row }">
          <span>{{ row.transportQuantity }}</span>
          <!-- <div class="clickable" v-empty-text @click.stop="openRecord(row, 'happened')">{{ row.transportQuantity }}</div> -->
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
import crudApi from '@/api/contract/sales-manage/order-tracking'
import { ref, provide, computed, nextTick } from 'vue'

import { projectStatusEnum, orderSourceTypeEnum } from '@enum-ms/contract'
import { orderTrackingPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import collectionRecord from './module/collection-record'
import invoiceRecord from './module/invoice-record'
import happenedRecord from './module/happened-record'
import warehouseRecord from './module/warehouse-record'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const currentView = computed(() => {
  if (recordType.value === 'collection') {
    return collectionRecord
  } else if (recordType.value === 'invoice') {
    return invoiceRecord
  } else if (recordType.value === 'warehouse') {
    return warehouseRecord
  }
  return happenedRecord
})

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '订单跟踪',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const detailInfo = ref({})
const productId = ref(undefined)
const recordType = ref('')
const recordVisible = ref(false)
const dataFormat = ref([
  ['project', 'parse-project'],
  ['orderSourceType', ['parse-enum', orderSourceTypeEnum]],
  ['status', ['parse-enum', projectStatusEnum]],
  ['contractAmount', 'to-thousand'],
  ['settlementAmount', 'to-thousand'],
  ['collectionAmount', 'to-thousand'],
  ['invoiceAmount', 'to-thousand'],
  ['happenedAmount', 'to-thousand'],
  ['warehouseAmount', 'to-thousand'],
  ['warehouseRate', ['to-fixed', 2]],
  ['collectionRate', ['to-fixed', 2]],
  ['invoiceRate', ['to-fixed', 2]],
  ['happenedRate', ['to-fixed', 2]]
])

provide('projectId', productId)

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    // 收款比例
    v.collectionRate = (v.settlementAmount || v.contractAmount) ? (v.collectionAmount || 0) / (v.settlementAmount || v.contractAmount) * 100 : 0
    // 开票比例
    v.invoiceRate = (v.settlementAmount || v.contractAmount) ? (v.invoiceAmount || 0) / (v.settlementAmount || v.contractAmount) * 100 : 0
    // 出库比例
    v.happenedRate = (v.settlementAmount || v.contractAmount) ? (v.happenedAmount || 0) / (v.settlementAmount || v.contractAmount) * 100 : 0
    // 入库比例
    v.warehouseRate = (v.settlementAmount || v.contractAmount) ? (v.warehouseAmount || 0) / (v.settlementAmount || v.contractAmount) * 100 : 0
  })
}

// 打开记录
function openRecord(row, type) {
  if (!checkPermission(permission.detail)) return
  detailInfo.value = row?.sourceRow
  recordType.value = type
  productId.value = row?.sourceRow?.project?.id
  nextTick(() => {
    recordVisible.value = true
  })
}

</script>
<style lang="scss" scoped>
.clickable {
  width: 100%;
  cursor: pointer;
}
</style>
