<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('project')" show-overflow-tooltip key="project" prop="project" label="项目" min-width="150">
        <template #default="{ row }">
          <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('projectContentName')" key="projectContentName" prop="projectContentName" label="合同内容" min-width="130" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.projectContentName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('contractAmount')" prop="contractAmount" key="contractAmount" label="合同额" align="right" min-width="120" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-thousand="row.contractAmount" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('settlementAmount')" prop="settlementAmount" key="settlementAmount" label="结算额" align="right" min-width="120" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-thousand="row.settlementAmount" v-empty-text />
        </template>
      </el-table-column>
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
          <el-tag effect="plain" type="success" class="clickable" v-thousand="row.collectionAmount" v-empty-text @click.stop="openRecord(row, 'collection')" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('collectionRate')" key="collectionRate" prop="collectionRate" label="收款比例" align="center" width="90">
        <template #default="{ row }">
          <span>{{ toFixed(row.collectionRate, 2) }}%</span>
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
          <el-tag effect="plain" type="warning" class="clickable" v-thousand="row.invoiceAmount" v-empty-text @click.stop="openRecord(row, 'invoice')" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceRate')" key="invoiceRate" prop="invoiceRate" label="开票比例" align="center" width="90">
        <template #default="{ row }">
          <span>{{ toFixed(row.invoiceRate, 2) }}%</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('happenedAmount')" prop="happenedAmount" key="happenedAmount" label="累计发运额" align="right" min-width="120" show-overflow-tooltip>
        <template v-if="checkPermission(permission.detail)" #header>
          <el-tooltip
            effect="light"
            placement="top"
            content="点击行可以查看详情"
          >
            <div style="display: inline-block">
              <span>累计发运额 </span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{ row }">
          <el-tag effect="plain" class="clickable" v-thousand="row.happenedAmount" v-empty-text @click.stop="openRecord(row, 'happened')" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('happenedRate')" key="happenedRate" prop="happenedRate" label="发运额比例" align="center" width="90">
        <template #default="{ row }">
          <span>{{ toFixed(row.happenedRate, 2) }}%</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('transportQuantity')" key="transportQuantity" prop="transportQuantity" label="运输车次" align="center" width="80" show-overflow-tooltip>
        <template #default="{ row }">
          <span v-empty-text>{{ row.transportQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center" width="80" :show-overflow-tooltip="true">
        <template #default="{ row }">
          <span v-empty-text>{{ projectStatusEnum.VL[row.status]}}</span>
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

import { projectStatusEnum } from '@enum-ms/contract'
import { orderTrackingPM as permission } from '@/page-permission/contract'
import checkPermission from '@/utils/system/check-permission'
import { toFixed } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import collectionRecord from './module/collection-record'
import invoiceRecord from './module/invoice-record'
import happenedRecord from './module/happened-record'

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

provide('projectId', productId)

// 刷新数据后
CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach(v => {
    // 收款比例
    v.collectionRate = v.contractAmount ? (v.collectionAmount || 0) / (v.contractAmount || 0) * 100 : 0
    // 开票比例
    v.invoiceRate = v.invoiceAmount ? (v.collectionAmount || 0) / (v.invoiceAmount || 0) * 100 : 0
    // 发运比例
    v.happenedRate = v.happenedAmount ? (v.collectionAmount || 0) / (v.happenedAmount || 0) * 100 : 0
  })
}

// 打开记录
function openRecord(row, type) {
  if (!checkPermission(permission.detail)) return
  detailInfo.value = row
  recordType.value = type
  productId.value = row?.project?.id
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
