<template>
  <div class="head-container">
    <div :style="flag ? 'display: block' : 'display: none'">
      <common-radio-button v-model="query.type" :options="timeTypeEnum.ENUM" class="filter-item" type="enum" @change="handleChange" />
      <el-date-picker
        v-if="query.type === timeTypeEnum.ALL_YEAR.V"
        v-model="query.dateTime"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width: 120px !important"
        placeholder="选择年"
        format="YYYY"
        value-format="x"
        :disabled-date="disabledDate"
        @change="handleYearChange"
      />
      <el-date-picker
        v-if="query.type === timeTypeEnum.CURRENT_MONTH.V"
        v-model="query.dateTime"
        type="month"
        size="small"
        class="date-item filter-item"
        style="width: 120px !important"
        placeholder="选择月"
        format="YYYY-MM"
        value-format="x"
        :disabled-date="disabledDate"
        @change="handleMonthChange"
      />
      <workshop-select
        ref="workshopInfRef"
        v-model="query.workShopId"
        placeholder="请选择车间"
        :workshop-type="workshopTypeEnum.BUILDING.V"
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
        <span>{{
          crud.query.weightStatus === weightTypeEnum.NET.V
            ? (yearProductionData.mete?.totalNetWeight / 1000).toFixed(DP.COM_WT__KG)
            : (yearProductionData.mete?.totalGrossWeight / 1000).toFixed(DP.COM_WT__KG)
        }}</span>
      </el-tag>
      <common-radio-button
        type="enum"
        v-model="query.weightStatus"
        :options="[weightTypeEnum.NET, weightTypeEnum.GROSS]"
        class="filter-item"
        style="float: right"
        @change="crud.toQuery"
      />
    </div>
    <div :style="flag ? 'display: block' : 'display: none'">
      <div style="width: 100%; height: 250px; display: flex" v-loading="loading">
        <div v-loading="loading" id="workshopDeliveryChart" style="width: 100%; height: 250px"></div>
      </div>
    </div>
    <el-divider :style="flag ? 'display: block' : 'display: none'" class="divider" />
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
        <!-- <project-cascader v-model="query.projectId" class="filter-item" @change="handleProjectIdChange" clearable style="width: 300px" /> -->
        <div class="icon-box" v-show="flag" style="vertical-align: middle" @click.stop="changeSize">
          <svg-icon class="icon" icon-class="comp-zoom" />
        </div>
        <div class="icon-box" v-show="!flag" style="vertical-align: middle" @click.stop="changeSize">
          <svg-icon class="icon" icon-class="comp-zoom-out" />
        </div>
        <el-input
          v-model.trim="query.projectName"
          size="small"
          placeholder="项目搜索"
          style="width: 170px"
          class="filter-item"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <!-- <el-tag type="success" class="filter-item" size="medium">
            <span>产量（吨）</span>
            <span>：</span>
            <span>{{
              crud.query.weightStatus === weightTypeEnum.NET.V
                ? (summaryList.mete?.totalNetWeight / 1000).toFixed(DP.COM_WT__KG)
                : (summaryList.mete?.totalGrossWeight / 1000).toFixed(DP.COM_WT__KG)
            }}</span>
          </el-tag> -->
        <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
        <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
          重置
        </common-button>
      </template>
      <template #viewLeft>
        <print-table
          v-permission="permission.print"
          api-key="mesFactoryWorkshopReport"
          :params="{ startTime: query.startTime, endTime: query.endTime, projectName: query.projectName }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, watch, reactive, defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'
import useChart from '@compos/use-chart'
import moment from 'moment'
import { DP } from '@/settings/config'
import { parseTime } from '@/utils/date'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
// import { getOrderDeliveryRate } from '@/api/operation/order-delivery-rate'
import { fullYearProduction, workshopEcharts } from '@/api/mes/factory-report/workshop-report.js'
import { weightTypeEnum, workshopTypeEnum } from '@enum-ms/common'
import { timeTypeEnum } from '@enum-ms/contract'
import { mesFactoryReportPM as permission } from '@/page-permission/mes'
import workshopSelect from '@comp-mes/workshop-select'
import productionLineSelect from '@comp-mes/production-line-select'
// import projectCascader from '@comp-base/project-cascader'
import crudOperation from '@crud/CRUD.operation'

const emit = defineEmits('zoomChangeSize')
const yearProductionData = reactive({
  mete: 0
})
// const summaryList = reactive({
//   mete: 0
// })
const chartDateTime = ref()
const chartYearTime = ref()
const chartVal = ref([])
const flag = ref(true)

const defaultQuery = {
  date: [moment().startOf('month').valueOf(), moment().valueOf()],
  startTime: moment().startOf('month').valueOf(),
  endTime: moment().valueOf(),
  type: timeTypeEnum.ALL_YEAR.V,
  dateTime: moment().startOf('year').valueOf(),
  projectId: undefined,
  workShopId: undefined,
  productionLineId: undefined,
  weightStatus: weightTypeEnum.NET.V,
  projectName: undefined
}
const { crud, query } = regHeader(defaultQuery)

function handleChange(val) {
  if (val === timeTypeEnum.ALL_YEAR.V) {
    query.dateTime = moment().startOf('year').valueOf()
  } else {
    query.dateTime = moment().startOf('month').valueOf()
  }
  fetchSummary()
  fetchChart()
  crud.toQuery()
}

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

fetchSummary()
// workshopSummary()

async function fetchSummary() {
  try {
    const data = await fullYearProduction({
      type: query.type,
      dateTime: query.dateTime ? query.dateTime : moment().startOf('year').valueOf(),
      workShopId: query.workShopId,
      productionLineId: query.productionLineId
    })
    yearProductionData.mete = data
  } catch (e) {
    console.log('获取全年产量失败', e)
  }
}

function handleWorkshopChange() {
  fetchSummary()
  fetchChart()
}

function handleProductionLineChange() {
  fetchSummary()
  fetchChart()
}

function handleYearChange() {
  fetchSummary()
  fetchChart()
  crud.toQuery()
}

function handleMonthChange() {
  fetchSummary()
  fetchChart()
  crud.toQuery()
}

function handleDateChange(val) {
  crud.query.date = val
  query.startTime = val[0]
  query.endTime = val[1]
  crud.toQuery()
}
// 搜索
function searchQuery() {
  fetchSummary()
  fetchChart()
  crud.toQuery()
}
// 重置
function resetQuery() {
  query.type = timeTypeEnum.ALL_YEAR.V
  query.dateTime = undefined
  query.workShopId = undefined
  query.productionLineId = undefined
  query.weightStatus = weightTypeEnum.NET.V
  query.projectName = undefined
  fetchSummary()
  fetchChart()
  crud.toQuery()
}

const workshopInfRef = ref()
const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}
const day = ref()
const dayArr = ref([])
watch([() => query.type, () => query.dateTime], (val) => {
  if (query.type === timeTypeEnum.CURRENT_MONTH.V) {
    day.value = moment(parseTime(query.dateTime, '{y}-{m}'), 'YYYY-MM').daysInMonth()
    dayArr.value = []
    for (let k = 1; k <= day.value; k++) {
      dayArr.value.push(k)
    }
  }
})

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
  chartVal.value = []
  try {
    const productionData = []
    chartLoading.value = true
    const _myChart = getMyChart()
    _myChart.on('click', function (params) {
      chartYearTime.value = crud.query.dateTime === undefined ? moment().valueOf() : query.dateTime
      if (crud.query.type === timeTypeEnum.ALL_YEAR.V) {
        chartDateTime.value = params.name?.split('')[0]
      } else {
        chartDateTime.value = params.name
      }
      chartDateTime.value = Number(chartDateTime.value) < 10 ? '0' + chartDateTime.value : chartDateTime.value
      if (crud.query.type === timeTypeEnum.ALL_YEAR.V) {
        chartYearTime.value = crud.query.dateTime === undefined ? moment().valueOf() : query.dateTime
        chartVal.value = [
          moment(parseTime(chartYearTime.value, '{y}') + '-' + chartDateTime.value)
            .startOf('month')
            .valueOf(),
          moment(parseTime(chartYearTime.value, '{y}') + '-' + chartDateTime.value)
            .endOf('month')
            .valueOf()
        ]
      } else {
        chartVal.value = [
          moment(parseTime(chartYearTime.value, '{y}-{m}') + '-' + chartDateTime.value)
            .startOf('date')
            .valueOf(),
          moment(parseTime(chartYearTime.value, '{y}-{m}') + '-' + chartDateTime.value)
            .endOf('date')
            .valueOf()
        ]
      }
      handleDateChange(chartVal.value)
    })
    const data = await workshopEcharts({
      type: query.type,
      dateTime: query.dateTime ? query.dateTime : moment().startOf('year').valueOf(),
      workShopId: query.workShopId,
      productionLineId: query.productionLineId
    })
    const option = _myChart.getOption()
    if (crud.query.type === timeTypeEnum.CURRENT_MONTH.V) {
      option.xAxis[0].data = dayArr.value
    } else {
      option.xAxis[0].data = monthArr.value
    }
    for (let i = 0; i < option.xAxis[0].data?.length; i++) {
      if (data.findIndex((k) => Number(k.date) === i + 1) > -1) {
        productionData.push({
          date: option.xAxis[0].data[i],
          totalNetWeight: data[data.findIndex((k) => Number(k.date) === i + 1)]?.totalNetWeight,
          totalGrossWeight: data[data.findIndex((k) => Number(k.date) === i + 1)]?.totalGrossWeight
        })
      } else {
        productionData.push({
          date: option.xAxis[0].data[i],
          totalNetWeight: 0,
          totalGrossWeight: 0
        })
      }
    }
    option.series[0].data = productionData.map((v) =>
      crud.query.weightStatus === weightTypeEnum.NET.V
        ? Number((v?.totalNetWeight / 1000).toFixed(DP.COM_WT__KG))
        : Number((v.totalGrossWeight / 1000).toFixed(DP.COM_WT__KG))
    )
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取车间报表信息失败')
  } finally {
    chartLoading.value = false
  }
}

function changeSize() {
  flag.value = !flag.value
  emit('zoomChangeSize', flag.value)
}
</script>

<style lang="scss" scoped>
.icon-box {
  position: relative;
  display: inline-flex;
  justify-content: center;
  align-items: center;
  width: 25px;
  height: 25px;
  cursor: pointer;
  background: rgb(83, 83, 82);
  border-radius: 50%;
  opacity: 0.5;
  margin-right: 5px;
  :hover {
    width: 80%;
    height: 80%;
  }
  &:hover {
    opacity: 1;
  }
}
.icon {
  position: absolute;
  width: 15px;
  height: 15px;
}
</style>
