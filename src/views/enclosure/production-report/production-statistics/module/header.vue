<template>
  <div>
    <common-radio-button
      v-model="query.category"
      :options="mesEnclosureTypeEnum.ENUM"
      type="enum"
      class="filter-item"
      size="small"
      @change="crud.toQuery"
    />
    <common-radio-button v-model="query.type" :options="timeTypeEnum.ENUM" class="filter-item" type="enumSL" @change="handleChange" />
    <el-date-picker
      v-if="query.type === timeTypeEnum.ALL_YEAR.V"
      v-model="query.time"
      type="year"
      size="small"
      :clearable="false"
      class="filter-item"
      style="width: 120px !important"
      placeholder="选择年"
      format="YYYY"
      value-format="x"
      :disabled-date="disabledDate"
    />
    <el-date-picker
      v-if="query.type === timeTypeEnum.CURRENT_MONTH.V"
      v-model="query.time"
      type="month"
      size="small"
      :clearable="false"
      class="filter-item"
      style="width: 120px !important"
      placeholder="选择月"
      format="YYYY-MM"
      value-format="x"
      :disabled-date="disabledDate"
    />
    <el-tag class="filter-item" size="medium" effect="plain" style="float: right">
      <span>全年累计产量（m）：{{ yearProduction || 0 }}</span>
    </el-tag>
    <div v-show="crud.searchToggle">
      <div v-loading="chartLoading" id="enclosureWorkshopReportChart" style="width: 100%; height: 250px; margin-bottom: 10px"></div>
    </div>
    <crudOperation>
      <template #optLeft>
        <el-date-picker
          v-model="query.date"
          type="daterange"
          range-separator=":"
          size="small"
          value-format="x"
          :shortcuts="PICKER_OPTIONS_SHORTCUTS"
          :clearable="false"
          unlink-panels
          start-placeholder="开始日期"
          end-placeholder="结束日期"
          style="width: 240px; margin-right: 10px"
          class="filter-item date-item"
          @change="handleDateChange"
        />
        <rrOperation />
      </template>
      <template #viewLeft>
        <print-table v-permission="permission.print" api-key="enclosureProductionStatistics" :params="{ ...query }" size="mini" type="warning" />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { fullYearProduction, workshopEcharts } from '@/api/enclosure/production-report/production-statistics'
import { ref, watch } from 'vue'

import moment from 'moment'
import { parseTime } from '@/utils/date'
import { timeTypeEnum } from '@enum-ms/contract'
import { mesEnclosureTypeEnum } from '@enum-ms/mes'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { mesFactoryReportPM as permission } from '@/page-permission/mes'

import useChart from '@compos/use-chart'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const yearProduction = ref(0)
const chartDateTime = ref()
const chartYearTime = ref()
const chartVal = ref([])
const chartLoading = ref(false)

const startTime = String(moment().startOf('year').valueOf())
const endTime = String(moment().valueOf())
const defaultQuery = {
  type: timeTypeEnum.ALL_YEAR.V,
  startTime,
  endTime,
  date: [startTime, endTime],
  time: startTime,
  category: mesEnclosureTypeEnum.PRESSED_PLATE.V
}
const { crud, query } = regHeader(defaultQuery)

// 时间状态改变
function handleChange(val) {
  if (val === timeTypeEnum.ALL_YEAR.V) {
    query.time = startTime
  } else {
    query.time = String(moment().startOf('month').valueOf())
  }
}

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

fetchSummary()
// 获取汇总数据
async function fetchSummary() {
  try {
    yearProduction.value = await fullYearProduction({
      time: query.time,
      category: query.category
    })
  } catch (error) {
    console.log('获取全年产量失败', error)
  }
}

// 改变日期
function handleDateChange(val = []) {
  query.date = val
  query.startTime = val[0]
  query.endTime = val[1]
  crud.toQuery()
}

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}
const day = ref()
const dayArr = ref([])

watch([() => query.type, () => query.time], (val) => {
  if (query.type === timeTypeEnum.CURRENT_MONTH.V) {
    // 更新天数列表
    day.value = moment(parseTime(query.time, '{y}-{m}'), 'YYYY-MM').daysInMonth()
    dayArr.value = []
    for (let k = 1; k <= day.value; k++) {
      dayArr.value.push(k + '日')
    }
  }
  fetchAll()
})

// 使用EChart
const { getMyChart } = useChart({
  elementId: 'enclosureWorkshopReportChart',
  fetchHook: fetchChart,
  initOption: {
    legend: { show: false },
    xAxis: { data: monthArr.value },
    title: {
      text: '单位：m'
    },
    yAxis: {
      axisTick: {
        show: false
      },
      axisLine: {
        show: true
      },
      splitLine: {
        show: false
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

// 获取图表数据
async function fetchChart() {
  chartVal.value = []
  try {
    const productionData = []
    chartLoading.value = true
    const _myChart = getMyChart()

    // EChart 点击
    _myChart.on('click', function (params) {
      chartYearTime.value = query.time
      if (crud.query.type === timeTypeEnum.ALL_YEAR.V) {
        chartDateTime.value = params.name.split('月')[0]
      } else {
        chartDateTime.value = params.name.split('日')[0]
      }
      chartDateTime.value = Number(chartDateTime.value) < 10 ? '0' + chartDateTime.value : chartDateTime.value

      if (crud.query.type === timeTypeEnum.ALL_YEAR.V) {
        const _date = parseTime(chartYearTime.value, '{y}') + '-' + chartDateTime.value
        chartVal.value = [moment(_date).startOf('month').valueOf(), moment(_date).endOf('month').valueOf()]
      } else {
        const _date = parseTime(chartYearTime.value, '{y}-{m}') + '-' + chartDateTime.value
        chartVal.value = [moment(_date).startOf('date').valueOf(), moment(_date).endOf('date').valueOf()]
      }
      handleDateChange(chartVal.value)
    })

    // 获取图表数据
    const data = await workshopEcharts({
      category: query.category,
      type: query.type,
      time: query.time
    })
    const dataKV = {}
    data.forEach((row) => {
      dataKV[row.date] = row.totalLength
    })

    const option = _myChart.getOption()
    if (crud.query.type === timeTypeEnum.CURRENT_MONTH.V) {
      option.xAxis[0].data = dayArr.value
    } else {
      option.xAxis[0].data = monthArr.value
    }

    const optionData = option.xAxis[0].data || []
    for (let i = 0; i < optionData.length; i++) {
      if (dataKV[i + 1]) {
        productionData.push({
          date: optionData[i],
          totalLength: dataKV[i + 1]
        })
      } else {
        productionData.push({
          date: optionData[i],
          totalLength: 0
        })
      }
    }
    option.series[0].data = productionData.map((v) => v.totalLength)
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取车间报表图表失败')
  } finally {
    chartLoading.value = false
  }
}

// 请求所有接口
function fetchAll() {
  fetchSummary()
  fetchChart()
  crud.toQuery()
}
</script>
