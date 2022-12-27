<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <div style="display: flex">
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        highlight-current-row
        returnSourceData
        row-key="id"
        style="width: 35%; cursor: pointer"
        show-summary
        :summary-method="getSummaries"
        @current-change="monthlyTaskChange"
      >
        <el-table-column
          v-if="columns.visible('month')"
          align="center"
          key="month"
          prop="month"
          :show-overflow-tooltip="true"
          label="月份"
          width="80px"
        >
          <template v-slot="scope">
            <span>{{ scope.row.month }}月</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('list')"
          align="center"
          key="list"
          prop="list"
          :show-overflow-tooltip="true"
          label="排产量（件/吨）"
        >
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}/{{ (scope.row.totalNetWeight / 1000).toFixed(DP.COM_WT__KG) }}</span>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('rate')" align="center" key="rate" prop="rate" :show-overflow-tooltip="true" label="达成率" width="160px">
          <template v-slot="scope">
            <span>
              <el-progress
                :text-inside="true"
                stroke-linecap="square"
                :stroke-width="22"
                :percentage="((scope.row.completeQuantity / scope.row.quantity) * 100).toFixed(2)"
                status="success"
              />
            </span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('complete')"
          align="center"
          key="complete"
          prop="complete"
          :show-overflow-tooltip="true"
          label="实际完成（件/吨）"
        >
          <template v-slot="scope">
            <span>{{ scope.row.completeQuantity }}/{{ (scope.row.completeNetWeight / 1000).toFixed(DP.COM_WT__KG) }}</span>
          </template>
        </el-table-column>
      </common-table>
      <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 180px)"></div>
      <monthly-task-detail :query="query" :monthly-data="monthlyData" style="flex: 1" />
    </div>
  </div>
</template>
<script setup>
import { ref, computed, watch } from 'vue'
import crudApi from '@/api/mes/task-tracking/monthly-task-tracking.js'
import { mesMonthlyTaskTrackingPM as permission } from '@/page-permission/mes'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { DP } from '@/settings/config'
// import { tableSummary } from '@/utils/el-extra'
import mHeader from './module/header.vue'
import monthlyTaskDetail from './monthly-task-detail/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const query = computed(() => {
  return {
    workShopId: crud.query.workShopId,
    productionLineId: crud.query.productionLineId
  }
})

const tableRef = ref()
const monthlyData = ref([])
const { crud, CRUD, columns } = useCRUD(
  {
    title: '月度任务跟踪',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

watch(
  () => crud.query.dateTime,
  (val) => {
    if (val) {
      monthlyData.value = {}
    }
  }
)
watch(
  () => query.value,
  (val) => {
    if (val) {
      monthlyData.value = {}
    }
  }
)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

function monthlyTaskChange(row) {
  monthlyData.value = row
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data
}

// 合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  const ave = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (index === 2) {
      sums[index] = 0
      data.map((v) => ave.push(v.completeQuantity / v.quantity))
      for (let i = 0; i <= ave.length - 1; i++) {
        sums[index] = sums[index] + ave[i]
      }
      sums[index] = ((sums[index] / ave.length) * 100).toFixed(2) + '%'
      return
    }
    if (column.property === 'list' || column.property === 'complete') {
      const valueKeys = column.property === 'list' ? 'quantity' : column.property + 'Quantity'
      const values = data.map((item) => Number(item?.[valueKeys]))
      let valuesSum = 0
      const valueWeightKeys = column.property === 'list' ? 'totalNetWeight' : column.property + 'NetWeight'
      const valueWeight = data.map((item) => Number(item?.[valueWeightKeys] / 1000))
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
      sums[index] = valuesSum + '/' + valueWeightSum.toFixed(DP.COM_WT__KG)
    }
  })
  return sums
}
</script>
<style lang="scss" scoped>
</style>
