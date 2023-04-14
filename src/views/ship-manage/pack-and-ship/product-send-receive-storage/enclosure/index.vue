<template>
  <div class="app-container">
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :stripe="false"
      :data-format="dataFormat"
      show-summary
      :summary-method="getSummaries"
      style="width: 100%"
    >
      <el-table-column type="index" prop="index" label="序号" align="center" width="60" />
      <el-table-column
        key="project"
        prop="project"
        v-if="columns.visible('project')"
        :show-overflow-tooltip="true"
        label="项目"
        align="left"
      />
      <el-table-column
        key="list"
        prop="list"
        v-if="columns.visible('list')"
        :show-overflow-tooltip="true"
        label="清单总数（件/米）"
        align="center"
      >
        <template v-slot="scope">
          <span>{{
            crud.query.weightStatus === weightTypeEnum.NET.V
              ? scope.row.quantity + ' / ' + scope.row.totalNetWeight
              : scope.row.quantity + ' / ' + scope.row.totalGrossWeight
          }}</span>
        </template>
      </el-table-column>
      <el-table-column
        key="beginning"
        prop="beginning"
        v-if="columns.visible('beginning')"
        :show-overflow-tooltip="true"
        label="期初库存（件/米）"
        align="center"
      >
        <template v-slot="scope">
          <span style="cursor: pointer; color: #0d84ff" @click="openDetail(scope.row, 'BEGINNING')">{{
            crud.query.weightStatus === weightTypeEnum.NET.V
              ? scope.row.beginningQuantity + ' / ' + scope.row.beginningNetWeight
              : scope.row.beginningQuantity + ' / ' + scope.row.beginningGrossWeight
          }}</span>
        </template>
      </el-table-column>
      <el-table-column
        key="inbound"
        prop="inbound"
        v-if="columns.visible('inbound')"
        :show-overflow-tooltip="true"
        label="入库量（件/米）"
        align="center"
      >
        <template v-slot="scope">
          <span style="cursor: pointer; color: #0d84ff" @click="openDetail(scope.row, 'INBOUND')">{{
            crud.query.weightStatus === weightTypeEnum.NET.V
              ? scope.row.inboundQuantity + ' / ' + scope.row.inboundNetWeight
              : scope.row.inboundQuantity + ' / ' + scope.row.inboundGrossWeight
          }}</span>
        </template>
      </el-table-column>
      <el-table-column
        key="outbound"
        prop="outbound"
        v-if="columns.visible('outbound')"
        :show-overflow-tooltip="true"
        label="出库量（件/米）"
        align="center"
      >
        <template v-slot="scope">
          <span style="cursor: pointer; color: #0d84ff" @click="openDetail(scope.row, 'OUTBOUND')">{{
            crud.query.weightStatus === weightTypeEnum.NET.V
              ? scope.row.outboundQuantity + ' / ' + scope.row.outboundNetWeight
              : scope.row.outboundQuantity + ' / ' + scope.row.outboundGrossWeight
          }}</span>
        </template>
      </el-table-column>
      <el-table-column
        key="stock"
        prop="stock"
        v-if="columns.visible('stock')"
        :show-overflow-tooltip="true"
        label="期末库存（件/米）"
        align="center"
      >
        <template v-slot="scope">
          <span style="cursor: pointer; color: #0d84ff" @click="openDetail(scope.row, 'STOCK')">{{
            crud.query.weightStatus === weightTypeEnum.NET.V
              ? scope.row.stockQuantity + ' / ' + scope.row.stockNetWeight
              : scope.row.stockQuantity + ' / ' + scope.row.stockGrossWeight
          }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="操作" width="130px" align="center" v-if="checkPermission([...permission.detail])" fixed="right">
        <template v-slot="scope">
          <common-button
            size="mini"
            icon="el-icon-view"
            type="primary"
            v-permission="permission.detail"
            @click="openDetail(scope.row, 'detail')"
          />
        </template>
      </el-table-column>
    </common-table>
    <pagination />
    <component
      :is="showComponent"
      :showType="showType"
      v-model="detailVisible"
      :detailQuery="detailQuery"
      :productType="crud.query.productType"
      :workshopId="crud.query.workshopId"
      :dateTime="crud.query.dateTime"
      :weightStatus="crud.query.weightStatus"
      :detailInfo="currentRow"
      :permission="permission"
    />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/pack-and-ship/product-receive-send-storage'
import { ref, nextTick, computed } from 'vue'

import { mesProductSendReceiveStoragePM as permission } from '@/page-permission/mes'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { DP } from '@/settings/config'
import checkPermission from '@/utils/system/check-permission'
import { weightTypeEnum } from '@enum-ms/common'
import mHeader from './module/header'
import pagination from '@crud/Pagination'
import mDetail from './module/detail'
import typeDetail from './module/type-detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const detailVisible = ref(false)
const showType = ref('detail')
const currentRow = ref({})
const detailQuery = ref({})

const showComponent = computed(() => {
  return showType.value === 'detail' ? mDetail : typeDetail
})

const { crud, columns } = useCRUD(
  {
    title: '制成品入发存',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: crudApi,
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.product-send-receive-storage',
  paginate: true,
  extraHeight: 40
})

const dataFormat = ref([['project', 'parse-project']])

function openDetail(row, show) {
  showType.value = show
  currentRow.value = row.sourceRow
  detailQuery.value = {
    projectId: row.sourceRow.project.id,
    dateTime: crud.query.dateTime
  }
  nextTick(() => {
    detailVisible.value = true
  })
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (
      column.property === 'list' ||
      column.property === 'inbound' ||
      column.property === 'outbound' ||
      column.property === 'stock' ||
      column.property === 'beginning'
    ) {
      const valueKeys = column.property === 'list' ? 'quantity' : column.property + 'Quantity'
      const values = data.map((item) => Number(item.sourceRow?.[valueKeys]))
      let valuesSum = 0
      // const valueWeightKeys = column.property === 'list' ? 'totalNetWeight' : column.property + 'NetWeight'
      let valueWeightKeys = ''
      if (column.property === 'list' && crud.query.weightStatus === weightTypeEnum.NET.V) {
        valueWeightKeys = 'totalNetWeight'
      } else if (column.property === 'list' && crud.query.weightStatus === weightTypeEnum.GROSS.V) {
        valueWeightKeys = 'totalGrossWeight'
      } else if (column.property !== 'list' && crud.query.weightStatus === weightTypeEnum.NET.V) {
        valueWeightKeys = column.property + 'NetWeight'
      } else if (column.property !== 'list' && crud.query.weightStatus === weightTypeEnum.GROSS.V) {
        valueWeightKeys = column.property + 'GrossWeight'
      }
      const valueWeight = data.map((item) => Number(item.sourceRow?.[valueWeightKeys]))
      let valueWeightSum = 0
      if (!values.every((value) => isNaN(value))) {
        valuesSum = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
      if (!valueWeight.every((value) => isNaN(value))) {
        valueWeightSum = valueWeight.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
      sums[index] = valuesSum + ' / ' + valueWeightSum.toFixed(DP.COM_WT__KG)
    }
  })
  return sums
}
</script>
<style lang="scss" scoped>
::v-deep(.el-table .abnormal-row) {
  background: #f0f9eb;
}
.customer-table {
  ::v-deep(th) {
    border: none;
  }
  ::v-deep(td) {
    border: none;
  }
  ::v-deep(th.is-leaf) {
    border: none;
  }
  &::before {
    width: 0;
  }
}
::v-deep(.el-progress-bar__inner) {
  text-align: center;
  max-width: 100%;
}
</style>
