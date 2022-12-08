<template>
  <div class="head-container">
    <div>
      <el-date-picker
        v-model="query.dateTime"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width: 120px !important"
        format="YYYY"
        :clearable="true"
        value-format="x"
        placeholder="选择年"
        :disabled-date="disabledDate"
        @change="handleDateTimeChange"
      />
      <workshop-select
        ref="workshopInfRef"
        v-model="query.workShopId"
        placeholder="请选择车间"
        :factory-id="query.factoryId"
        style="width: 200px"
        class="filter-item"
        clearable
        @change="handleWorkshopChange"
      />
      <production-line-select
        ref="productionLineRef"
        class="filter-item"
        v-model="query.productionLineId"
        :factory-id="query.factoryId"
        :workshop-id="query.workShopId"
        placeholder="请选择生产线"
        style="width: 200px"
        clearable
        @change="handleProductionLineChange"
      />
      <el-tag class="filter-item" size="medium">
        <span>全年累计产量（吨）</span>
        <span>：</span>
        <span>{{ (yearProductionData.mete / 1000).toFixed(DP.COM_WT__KG) }}</span>
      </el-tag>
      <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button>
    </div>
    <div>
      <div style="width: 100%; height: 250px; display: flex" v-loading="loading">
        <div v-loading="loading" id="workshopDeliveryChart" style="width: 100%; height: 250px"></div>
      </div>
    </div>
    <el-divider class="divider" />
    <div style="margin-top: 10px">
      <crudOperation>
        <template #optLeft>
          <el-date-picker
            v-model="query.date"
            type="daterange"
            range-separator=":"
            size="small"
            value-format="x"
            :shortcuts="PICKER_OPTIONS_SHORTCUTS"
            unlink-panels
            start-placeholder="开始日期"
            end-placeholder="结束日期"
            style="width: 240px; margin-right: 10px"
            class="filter-item date-item"
            @change="handleDateChange"
          />
          <project-cascader v-model="query.projectId" class="filter-item" @change="handleProjectIdChange" clearable style="width: 300px" />
          <el-tag type="success" class="filter-item" size="medium">
            <span>产量（吨）</span>
            <span>：</span>
            <span>{{ (summaryList.mete / 1000).toFixed(DP.COM_WT__KG) }}</span>
          </el-tag>
        </template>
        <template #viewLeft>
          <print-table
          v-permission="permission.print"
            api-key="bridgeFactoryWorkshopReport"
            :params="{ startTime: query.startTime, endTime: query.endTime, projectId: query.projectId }"
            size="mini"
            type="warning"
            class="filter-item"
          />
        </template>
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { ref, reactive } from 'vue'
import { regHeader } from '@compos/use-crud'
import useChart from '@compos/use-chart'
import moment from 'moment'
import { DP } from '@/settings/config'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
// import { getOrderDeliveryRate } from '@/api/operation/order-delivery-rate'
import { fullYearProduction, workshopEcharts, workshopProduction } from '@/api/bridge/bridge-workshop-report/workshop-report'
import { bridgeFactoryReportPM as permission } from '@/page-permission/bridge'
import workshopSelect from '@comp-mes/workshop-select'
import productionLineSelect from '@comp-mes/production-line-select'
import projectCascader from '@comp-base/project-cascader'
import crudOperation from '@crud/CRUD.operation'

const yearProductionData = reactive({
  mete: 0
})
const summaryList = reactive({
  mete: 0
})
const defaultQuery = {
  dateTime: undefined,
  date: [moment().startOf('month').valueOf(), moment().valueOf()],
  startTime: moment().startOf('month').valueOf(),
  endTime: moment().valueOf(),
  projectId: undefined,
  workShopId: undefined,
  productionLineId: undefined
}
const { crud, query } = regHeader(defaultQuery)

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

fetchSummary()
workshopSummary()

async function fetchSummary() {
  try {
    const data = await fullYearProduction({
      dateTime: query.dateTime,
      workShopId: query.workShopId,
      productionLineId: query.productionLineId
    })
    yearProductionData.mete = data
  } catch (e) {
    console.log('获取全年产量失败', e)
  }
}

async function workshopSummary() {
  try {
    const data = await workshopProduction({
      startTime: query.startTime,
      endTime: query.endTime,
      projectId: query.projectId
    })
    summaryList.mete = data
  } catch (e) {
    console.log('获取车间产量失败', e)
  }
}
function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startTime = query.date[0]
    query.endTime = query.date[1]
  } else {
    query.startTime = undefined
    query.endTime = undefined
  }
  workshopSummary()
  crud.toQuery()
}

function handleWorkshopChange() {
  fetchSummary()
  fetchChart()
}
function handleProductionLineChange() {
  fetchSummary()
  fetchChart()
}

function handleProjectIdChange() {
  workshopSummary()
  crud.toQuery()
}

// 搜索
function searchQuery() {
  fetchSummary()
  fetchChart()
}
// 重置
function resetQuery() {
  query.dateTime = undefined
  query.workShopId = undefined
  query.productionLineId = undefined
  fetchSummary()
  fetchChart()
}

function handleDateTimeChange() {
  query.workShopId = undefined
  query.productionLineId = undefined
  fetchSummary()
  fetchChart()
}
const workshopInfRef = ref()
const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const loading = ref(false)
const chartLoading = ref(false)

const { getMyChart } = useChart({
  elementId: 'workshopDeliveryChart',
  fetchHook: fetchChart,
  initOption: {
    legend: { show: false },
    xAxis: { data: monthArr.value },
    title: {
      text: '单位：吨',
      left: '90%',
      textStyle: {
        fontSize: 14
      }
    },
    yAxis: {
      axisTick: {
        show: false
      },
      axisLine: {
        show: true
      },
      splitLine: {
        show: true
      }
    },
    series: [
      {
        name: '产量',
        type: 'bar',
        yAxisIndex: 0,
        data: []
      }
    ]
  }
})

async function fetchChart() {
  try {
    chartLoading.value = true
    const _myChart = getMyChart()
    const productionData = []
    const data = await workshopEcharts({
      dateTime: query.dateTime,
      workShopId: query.workShopId,
      productionLineId: query.productionLineId
    })
    const option = _myChart.getOption()
    for (const i in data) {
      productionData.push(data[i])
    }
    console.log(productionData)
    option.series[0].data = productionData.map((v) => (v && Number((v / 1000).toFixed(DP.COM_WT__KG))) || 0)
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取车间报表信息失败')
  } finally {
    chartLoading.value = false
  }
}
</script>

<style>
</style>
