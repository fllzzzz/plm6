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
        style="width: 47%; cursor: pointer"
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
          v-if="columns.visible('quantity')"
          align="center"
          key="quantity"
          prop="quantity"
          :show-overflow-tooltip="true"
          label="排产数（件）"
        >
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('netWeight')"
          align="center"
          key="netWeight"
          prop="netWeight"
          :show-overflow-tooltip="true"
          label="排产总净重（吨）"
        >
          <template v-slot="scope">
            <span>{{ (scope.row.netWeight / 1000).toFixed(DP.COM_WT__KG) }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('grossWeight')"
          align="center"
          key="grossWeight"
          prop="grossWeight"
          :show-overflow-tooltip="true"
          label="排产总毛重（吨）"
        >
          <template v-slot="scope">
            <span>{{ (scope.row.grossWeight / 1000).toFixed(DP.COM_WT__KG) }}</span>
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
          v-if="columns.visible('completeQuantity')"
          align="center"
          key="completeQuantity"
          prop="completeQuantity"
          :show-overflow-tooltip="true"
          label="实际完成数（件）"
        >
          <template v-slot="scope">
            <span>{{ scope.row.completeQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('completeNetWeight')"
          align="center"
          key="completeNetWeight"
          prop="completeNetWeight"
          :show-overflow-tooltip="true"
          label="实际完成总净重（吨）"
        >
          <template v-slot="scope">
            <span>{{ (scope.row.completeNetWeight / 1000).toFixed(DP.COM_WT__KG) }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('completeGrossWeight')"
          align="center"
          key="completeGrossWeight"
          prop="completeGrossWeight"
          :show-overflow-tooltip="true"
          label="实际完成总毛重（吨）"
        >
          <template v-slot="scope">
            <span>{{ (scope.row.completeGrossWeight / 1000).toFixed(DP.COM_WT__KG) }}</span>
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
      sums[index] = '合计'
      return
    }
    if (index === 4) {
      sums[index] = 0
      data.map((v) => ave.push(v.completeQuantity / v.quantity))
      for (let i = 0; i <= ave.length - 1; i++) {
        sums[index] = sums[index] + ave[i]
      }
      sums[index] = ((sums[index] / ave.length) * 100).toFixed(2) + '%'
      return
    }
    if (index !== 0 && index !== 4 && index !== 1 && index !== 5) {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr / 1000
          } else {
            return prev
          }
        }, 0)
      }
      sums[index] = sums[index]?.toFixed(2)
    }
    if (index === 1 || index === 5) {
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
