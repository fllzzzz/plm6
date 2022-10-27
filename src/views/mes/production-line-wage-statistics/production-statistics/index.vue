<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <div style="display: flex">
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="productionData"
        highlight-current-row
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        row-key="projectId"
        style="width: 40%"
        show-summary
        :summary-method="getSummaries"
        @current-change="handleCurrentChange"
      >
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column align="center" key="process" prop="process" :show-overflow-tooltip="true" label="工序" min-width="80px">
          <template v-slot="scope">
            <span>{{ scope.row.process }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('production')"
          align="center"
          key="production"
          prop="production"
          :show-overflow-tooltip="true"
          label="产量（吨）"
          min-width="60px"
        >
          <template v-slot="scope">
            <span>{{ scope.row.production }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('sum')"
          align="center"
          key="sum"
          prop="sum"
          :show-overflow-tooltip="true"
          label="计件总额（元）"
          min-width="60px"
        >
          <template v-slot="scope">
            <span>{{ scope.row.sum }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('average')"
          align="center"
          key="average"
          prop="average"
          :show-overflow-tooltip="true"
          label="平均（元/吨）"
          min-width="60px"
        >
          <template v-slot="scope">
            <span>{{ scope.row.average }}</span>
          </template>
        </el-table-column>
      </common-table>
      <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 180px)"></div>
      <production-detail :detail-row="detailRow" style="flex: 1" />
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { tableSummary } from '@/utils/el-extra'
import mHeader from './module/header'
import productionDetail from './production-detail/index.vue'

const tableRef = ref()
const detailRow = ref({})
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})
const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const productionData = [
  { id: 1, process: '下料', production: 100, sum: 100, average: 1 },
  { id: 2, process: '组立', production: 200, sum: 1000, average: 1 },
  { id: 3, process: '埋弧', production: 300, sum: 200, average: 1 },
  { id: 4, process: '焊接', production: 400, sum: 1000, average: 1 }
]

const { crud, columns } = useCRUD(
  {
    title: '产量统计',
    sort: [],
    optShow: { ...optShow },
    //   crudApi: { ...crudApi },
    // permission: { ...permission },
    hasPagination: true
  },
  tableRef
)

// 求和
function getSummaries(param) {
  return tableSummary(param, {
    props: ['sum']
  })
}
function handleCurrentChange(row) {
  detailRow.value = row
}
</script>

<style lang="scss" scoped>
</style>
