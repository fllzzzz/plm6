<template>
  <div class="head-container">
    <div>
      <el-date-picker
        v-model="query.year"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width: 100px !important"
        format="YYYY"
        value-format="YYYY"
        placeholder="选择年"
        :disabled-date="disabledDate"
        @change="crud.toQuery"
      />
      <workshop-select
        ref="workshopInfRef"
        v-model="query.workshopInfId"
        placeholder="请选择车间"
        :factory-id="query.factoryId"
        style="width: 270px"
        class="filter-item"
        defaultValue
      />
      <production-line-select
        ref="productionLineRef"
        class="filter-item"
        v-model="query.productionLineId"
        :factory-id="query.factoryId"
        placeholder="请选择生产线"
        style="width: 270px"
        clearable
        defaultValue
      />
      <el-tag effect="plain" class="filter-item" size="medium"> <span>全年累计产量（吨）</span> : <span>444</span> </el-tag>
      <rrOperation />
    </div>
    <div>
      <div style="width: 100%; height: 250px; display: flex" v-loading="loading">
        <div v-loading="loading" id="orderDeliveryRateChart" style="width: 100%; height: 250px"></div>
      </div>
    </div>
    <el-divider class="divider" />
    <div style="margin-top: 10px">
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
      <project-cascader v-model="query.projectId" class="filter-item" />
      <el-tag effect="success" class="filter-item" size="medium"> <span>产量（吨）</span> : <span>1200</span> </el-tag>
      <crudOperation>
        <template #optLeft>
          <print-table api-key="workshopReport" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
        </template>
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import { parseTime } from '@/utils/date'
import useChart from '@compos/use-chart'
// import useMaxHeight from '@compos/use-max-height'
import moment from 'moment'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { getOrderDeliveryRate } from '@/api/operation/order-delivery-rate'
import workshopSelect from '@comp-mes/workshop-select'
import productionLineSelect from '@comp-mes/production-line-select'
import projectCascader from '@comp-base/project-cascader'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  year: parseTime(new Date(), '{y}'),
  date: [moment().subtract(1, 'month').valueOf(), moment().valueOf()],
  startDate: moment().subtract(1, 'month').valueOf(),
  endDate: moment().valueOf(),
  projectId: undefined
}

const { crud, query } = regHeader(defaultQuery)

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startDate = query.date[0]
    query.endDate = query.date[1]
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}
const workshopInfRef = ref()
const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const year = ref(moment().valueOf().toString())
const loading = ref(false)
const chartLoading = ref(false)
const summaryInfo = ref({})

const { getMyChart } = useChart({
  elementId: 'orderDeliveryRateChart',
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
        name: '计划',
        type: 'bar',
        yAxisIndex: 0,
        data: []
      },
      {
        name: '实际',
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
    const {
      details = [],
      completeSummary = 0,
      avgDeliverRatio = 0,
      orderSummary = 0
    } = await getOrderDeliveryRate({ dateTime: year.value })
    const option = _myChart.getOption()
    summaryInfo.value = {
      completeSummary,
      avgDeliverRatio,
      orderSummary,
      totalRatio: (completeSummary / orderSummary) * 100
    }
    option.series[0].data = details.map((v) => (v.order && Number((v.order / 1000).toFixed(2))) || 0)
    // option.series[1].data = details.map((v) => {
    //   const value = (v.complete && Number((v.complete / 1000).toFixed(2))) || 0
    //   return {
    //     value,
    //     itemStyle: {
    //       color: value && !v.order ? '#cccccc' : '#91cc75'
    //     }
    //   }
    // })
    // option.series[2].data = details.map((v) => v.deliverRatio || 0)
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取车间产量信息失败')
  } finally {
    chartLoading.value = false
  }
}
</script>

<style>
</style>
