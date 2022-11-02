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
          v-if="columns.visible('totalQuantity')"
          align="center"
          key="totalQuantity"
          prop="totalQuantity"
          :show-overflow-tooltip="true"
          label="排产量（件/吨）"
        >
          <template v-slot="scope">
            <span>{{ scope.row.totalQuantity }}/{{ (scope.row.totalMete / 1000).toFixed(2) }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('rate')"
          align="center"
          key="rate"
          prop="rate"
          :show-overflow-tooltip="true"
          label="达成率"
        >
          <template v-slot="scope">
            <span>
              <el-progress
                :text-inside="true"
                stroke-linecap="square"
                :stroke-width="22"
                :percentage="scope.row.rate"
                status="success"
              />
            </span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('completeQuantity')"
          align="center"
          key="completeQuantity"
          prop="completeQuantity"
          :show-overflow-tooltip="true"
          label="实际完成（件/吨）"
        >
          <template v-slot="scope">
            <span>{{ scope.row.completeQuantity }}/{{ (scope.row.completeMete / 1000).toFixed(2)}}</span>
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
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
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
    // permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

watch(
  () => crud.query.dateTime,
  (val) => {
    if(val) {
      monthlyData.value = {}
    }
  }
)
watch(
  () => query.value,
  (val) => {
    if(val) {
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
      data.map((v) =>
        ave.push(v.rate)
      )
      for (let i = 0; i <= ave.length - 1; i++) {
        sums[index] = sums[index] + ave[i]
      }
      sums[index] = (sums[index] / ave.length).toFixed(2) + '%'
      return
    }
    if (column.property === 'totalQuantity' || column.property === 'completeQuantity') {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
    }
  })
  return sums
}
</script>
<style lang="scss" scoped>
</style>
