<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <div style="display: flex">
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="plateList"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        highlight-current-row
        returnSourceData
        row-key="projectId"
        style="width: 100%; flex: 2; cursor: pointer"
        show-summary
        :summary-method="getSummaries"
        @current-change="monthlyTaskChange"
      >
        <el-table-column prop="month" label="月份" align="center" width="60">
          <template v-slot="scope">
            <span>{{ scope.row.month }}</span>
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
            <span>{{ scope.row.totalQuantity }}/{{ scope.row.totalWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('fulfillmentRate')"
          align="center"
          key="fulfillmentRate"
          prop="fulfillmentRate"
          :show-overflow-tooltip="true"
          label="达成率"
        >
          <template v-slot="scope">
            <span>
              <el-progress
                :text-inside="true"
                stroke-linecap="square"
                :stroke-width="22"
                :percentage="scope.row.fulfillmentRate"
                status="success"
              />
            </span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('actualQuantity')"
          align="center"
          key="actualQuantity"
          prop="actualQuantity"
          :show-overflow-tooltip="true"
          label="实际完成（件/吨）"
        >
          <template v-slot="scope">
            <span>{{ scope.row.actualQuantity }}/{{ scope.row.actualWeight }}</span>
          </template>
        </el-table-column>
      </common-table>
      <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 180px)"></div>
      <monthly-task-detail :monthly-data="monthlyData" style="flex: 4" />
    </div>
  </div>
</template>
<script setup>
import { ref } from 'vue'
// import crudApi from ''
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import mHeader from './module/header.vue'
import monthlyTaskDetail from './monthly-task-detail/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const monthArr = ref([])

for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const plateList = [
  { totalQuantity: 200, totalWeight: 1000, fulfillmentRate: 36, actualQuantity: 100, actualWeight: 1000, month: '1月' },
  { totalQuantity: 200, totalWeight: 1000, fulfillmentRate: 30, actualQuantity: 80, actualWeight: 1000, month: '2月' },
  { totalQuantity: 200, totalWeight: 1000, fulfillmentRate: 46, actualQuantity: 80, actualWeight: 1000, month: '3月' }
]
const tableRef = ref()
const monthlyData = ref([])

const { crud, columns } = useCRUD(
  {
    title: '月度任务跟踪',
    sort: [],
    optShow: { ...optShow },
    // permission: { ...permission },
    // crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

function monthlyTaskChange(row) {
  monthlyData.value = row
}

// 合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (column.property === 'totalQuantity' || column.property === 'actualQuantity' || column.property === 'fulfillmentRate') {
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
