<template>
  <div class="app-container">
    <div style="display: flex">
      <div style="width: 38%">
        <div class="head-container">
          <mHeader />
        </div>
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="crud.data"
          :empty-text="crud.emptyText"
          :max-height="maxHeight"
          highlight-current-row
          returnSourceData
          row-key="id"
          style="cursor: pointer"
          show-summary
          :summary-method="getSummaries"
          @row-click="monthlyTaskChange"
        >
          <el-table-column
            v-if="columns.visible('months')"
            align="center"
            key="months"
            prop="months"
            :show-overflow-tooltip="true"
            label="月份"
            width="80px"
          >
            <template v-slot="scope">
              <span>{{ scope.row.months }}月</span>
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
              <span>{{
                crud.query.weightStatus === weightTypeEnum.NET.V
                  ? scope.row.quantity + '/' + (scope.row.netWeight / 1000).toFixed(DP.COM_WT__KG)
                  : scope.row.quantity + '/' + (scope.row.grossWeight / 1000).toFixed(DP.COM_WT__KG)
              }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('rate')"
            align="center"
            key="rate"
            prop="rate"
            :show-overflow-tooltip="true"
            label="达成率"
            width="160px"
          >
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
            label="实际完成量（件/吨）"
          >
            <template v-slot="scope">
              <span>{{
                crud.query.weightStatus === weightTypeEnum.NET.V
                  ? scope.row.completeQuantity + '/' + (scope.row.completeNetWeight / 1000).toFixed(DP.COM_WT__KG)
                  : scope.row.completeQuantity + '/' + (scope.row.completeGrossWeight / 1000).toFixed(DP.COM_WT__KG)
              }}</span>
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 180px)"></div>
      <monthly-task-detail :query="query" :weightStatus="crud.query.weightStatus" :monthly-data="monthlyData" style="width: 60%" />
    </div>
  </div>
</template>
<script setup>
import { ref, computed, watch } from 'vue'
import crudApi from '@/api/mes/task-tracking/monthly-task-tracking.js'
import { mesMonthlyTaskTrackingPM as permission } from '@/page-permission/mes'
import { weightTypeEnum } from '@enum-ms/common'
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
    invisibleColumns: ['grossWeight', 'completeGrossWeight'],
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

watch(
  [() => crud.query.dateTime, () => crud.query.weightStatus],
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
  res.data.content = res.data?.map((v) => {
    v.months = v.month?.split('-')[1]
    return v
  })
}

// 合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  const ave = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '全年平均'
      return
    }
    if (column.property === 'rate') {
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
      // const valueWeightKeys = column.property === 'list' ? 'netWeight' : column.property + 'NetWeight'
      let valueWeightKeys = ''
      if (column.property === 'list' && crud.query.weightStatus === weightTypeEnum.NET.V) {
        valueWeightKeys = 'netWeight'
      } else if (column.property === 'list' && crud.query.weightStatus === weightTypeEnum.GROSS.V) {
        valueWeightKeys = 'grossWeight'
      } else if (column.property !== 'list' && crud.query.weightStatus === weightTypeEnum.NET.V) {
        valueWeightKeys = column.property + 'NetWeight'
      } else if (column.property !== 'list' && crud.query.weightStatus === weightTypeEnum.GROSS.V) {
        valueWeightKeys = column.property + 'GrossWeight'
      }
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
