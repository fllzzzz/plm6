<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <div style="display: flex">
      <div style="width: 35%">
        <div style="margin-bottom: 8px">
          <el-date-picker
            v-model="date"
            type="daterange"
            range-separator=":"
            size="small"
            value-format="x"
            :shortcuts="PICKER_OPTIONS_DATE"
            unlink-panels
            start-placeholder="开始日期"
            end-placeholder="结束日期"
            style="width: 240px; margin-right: 10px"
            class="filter-item date-item"
            @change="handleDateChange"
          />
          <workshop-select
            v-model="workshopId"
            :workshop-type="workshopTypeEnum.BUILDING.V"
            placeholder="请选择车间"
            clearable
            style="width: 160px"
            class="filter-item"
            @change="fetchProcessData"
          />
        </div>
        <common-table
          ref="tableRef"
          v-loading="crud.loading"
          :data="productionData"
          highlight-current-row
          :empty-text="'暂无数据'"
          :max-height="maxHeight"
          row-key="projectId"
          style="width: 100%"
          show-summary
          :summary-method="getSummaries"
          @row-click="handleChange"
        >
          <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
          <el-table-column
            align="center"
            key="process.name"
            prop="process.name"
            :show-overflow-tooltip="true"
            label="工序"
            min-width="60px"
          >
            <template v-slot="scope">
              <table-cell-tag
                :name="componentTypeEnum.VL[scope.row.taskTypeEnum]"
                :color="componentTypeEnum.V[scope.row.taskTypeEnum].COLOR"
                :offset="10"
              />
              <span>{{ scope.row.process?.name }}</span>
            </template>
          </el-table-column>
          <el-table-column align="center" key="mete" prop="mete" :show-overflow-tooltip="true" label="产量（吨）" min-width="60px">
            <template v-slot="scope">
              <span>{{ scope.row.mete? (scope.row.mete / 1000)?.toFixed(2) : 0 }}</span>
            </template>
          </el-table-column>
          <el-table-column align="center" key="price" prop="price" :show-overflow-tooltip="true" label="工资总额（元）" min-width="60px">
            <template v-slot="scope">
              <span>{{ (scope.row.price)?.toFixed(2) }}</span>
            </template>
          </el-table-column>
          <el-table-column
            align="center"
            key="avgPrice"
            prop="avgPrice"
            :show-overflow-tooltip="true"
            label="平均单价（元/吨）"
            min-width="60px"
          >
            <template v-slot="scope">
              <span>{{ scope.row.mete? ((scope.row.price)?.toFixed(2) / (scope.row.mete / 1000)?.toFixed(2)).toFixed(2) : 0 }}</span>
            </template>
          </el-table-column>
        </common-table>
        <!-- 分页 -->
        <!-- <el-pagination
          :total="total"
          :current-page="queryPage.pageNumber"
          :page-size="queryPage.pageSize"
          style="margin-top: 8px"
          layout="total, prev, pager, next, sizes"
          @size-change="handleSizeChange"
          @current-change="handleCurrentChange"
        /> -->
      </div>
      <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 300px)"></div>
      <production-detail :commonParams="commonParams" :detail-row="detailRow" style="flex: 1; overflow-x: hidden" />
    </div>
  </div>
</template>

<script setup>
import { ref, onMounted, computed, watch, nextTick, provide } from 'vue'
import { get } from '@/api/mes/production-line-wage-statistics/production-statistics'
import checkPermission from '@/utils/system/check-permission'
import { mesProductionStatisticsPM as permission } from '@/page-permission/mes'
import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { DP } from '@/settings/config'
// import usePagination from '@compos/use-pagination'
// import { tableSummary } from '@/utils/el-extra'
import { ElNotification } from 'element-plus'
import { workshopTypeEnum } from '@enum-ms/common'
import { componentTypeEnum } from '@enum-ms/mes'
import { PICKER_OPTIONS_DATE } from '@/settings/config'
import workshopSelect from '@comp-mes/workshop-select'
import moment from 'moment'
import mHeader from './module/header'
import productionDetail from './production-detail/index.vue'

const tableRef = ref()
const productionData = ref([])
const detailRow = ref({})
const date = ref([moment().startOf('month').valueOf(), moment().valueOf()])
const startTime = ref(moment().startOf('month').valueOf().valueOf())
const endTime = ref(moment().valueOf())
const workshopId = ref()
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

const { crud } = useCRUD({
  title: '产量统计',
  sort: [],
  permission: { ...permission },
  optShow: { ...optShow },
  hasPagination: true
})

provide('permission', permission)
const commonParams = computed(() => {
  return {
    workshopId: workshopId.value,
    startTime: startTime.value,
    endTime: endTime.value
  }
})

watch(
  () => commonParams.value,
  (val) => {
    detailRow.value = {}
  }
)

// const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchProcessData })

onMounted(() => {
  fetchProcessData()
})

async function fetchProcessData() {
  startTime.value === date.value[0] ? startTime.value : undefined
  endTime.value === date.value[1] ? endTime.value : undefined
  if (!checkPermission(permission.get)) return
  try {
    const { content } = await get({
      workshopId: workshopId.value,
      startTime: startTime.value,
      endTime: endTime.value
    })
    // setTotalPage(totalElements)
    productionData.value = content || []
  } catch (error) {
    console.log('获取工序汇总信息失败', error)
  }
}

function handleDateChange(val) {
  if (val) {
    startTime.value = val[0]
    endTime.value = val[1]
    if (val[1] - val[0] > 24 * 60 * 60 * 1000 * 31) {
      val = []
      nextTick(() => {
        date.value = []
      })
      ElNotification({ title: `选择日期间隔最大不能超过31天`, type: 'error', duration: 2000 })
    } else {
      fetchProcessData()
    }
  } else {
    fetchProcessData()
  }
}

function handleChange(row) {
  detailRow.value = row
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
    if (column.property === 'mete') {
      const values = data.map((item) => Number(item[column.property]))
      let valuesSum = 0
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
      sums[index] = valuesSum ? (valuesSum / 1000)?.toFixed(2) : 0
    }
    if (column.property === 'price') {
      const values = data.map((item) => Number(item[column.property]))
      let valuesSum = 0
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
      sums[index] = valuesSum?.toFixed(DP.YUAN)
    }
  })
  return sums
}
</script>

<style lang="scss" scoped>
</style>
