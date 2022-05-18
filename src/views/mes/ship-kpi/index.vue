<template>
  <div class="app-container">
    <div class="head-container">
      <el-date-picker
        v-model="year"
        type="year"
        size="small"
        style="width: 300px"
        placeholder="选择年"
        value-format="x"
        :disabled-date="disabledDate"
        @change="refreshAll"
      />
    </div>
    <div class="flex-r">
      <div class="view-left">
        <div class="view-left-summary">
          <panel name="年度累计发运量" num-color="#1890ff" :end-val="summaryInfo.yearMete" style="margin-bottom: 5px" :precision="2" />
          <panel name="年度使用车次" num-color="#1890ff" :end-val="summaryInfo.yearQuantity" />
        </div>
        <div class="flex-c">
          <div v-loading="monthShipLoading" :style="{ height: maxLeftHeight / 2 + 'px' }" style="margin-top: 15px">
            <div id="monthShipChart" style="width: 100%; height: 100%"></div>
          </div>
          <div v-loading="carModelLoading" :style="{ height: maxLeftHeight / 2 + 'px' }" style="margin-top: 15px">
            <div id="carModelChart" style="width: 100%; height: 100%"></div>
          </div>
        </div>
      </div>
      <div class="view-center">
        <el-descriptions direction="vertical" :column="4" border class="shipment-summary">
          <el-descriptions-item align="center" label="昨日发运量">{{ summaryInfo.lastDayMete }}</el-descriptions-item>
          <el-descriptions-item align="center" label="今日发运量">{{ summaryInfo.todayMete }}</el-descriptions-item>
          <el-descriptions-item align="center" label="昨日使用车次">{{ summaryInfo.lastDayQuantity }}</el-descriptions-item>
          <el-descriptions-item align="center" label="今日使用车次">{{ summaryInfo.todayQuantity }}</el-descriptions-item>
        </el-descriptions>
        <common-table
          v-loading="projectYearShipLoading"
          :data="projectYearShipList"
          :height="maxCenterHeight"
          style="width: 100%; margin-top: 15px"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column prop="project.shortName" :show-overflow-tooltip="true" label="所属项目">
            <template #default="{ row }">
              <span class="project-name">{{
                projectNameFormatter(row.project, null, false)
              }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="totalWeight" :show-overflow-tooltip="true" label="清单量" align="center" width="100">
            <template #default="{ row }">
              <span>{{ row.totalWeight }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="stock" :show-overflow-tooltip="true" label="库存量" align="center" width="100">
            <template #default="{ row }">
              <span>{{ row.stock }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="shipWeight" :show-overflow-tooltip="true" label="已发运" align="center">
            <template #default="{ row }">
              <el-progress :text-inside="true" :stroke-width="26" :percentage="row.shipRate">
                <template #default="{ percentage }">
                  <span>{{ row.shipWeight }}</span> | <span>{{ toFixed(percentage, 2) }}%</span>
                </template>
              </el-progress>
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div class="view-right">
        <div class="view-right-summary" style="margin-bottom: 15px">
          <panel name="当前项目制成品库存" num-color="#1890ff" :end-val="summaryInfo.currentStock" :precision="2" />
        </div>
        <div class="view-line-title">
          <span style="font-size: 18px">今日发运</span>
          <span style="font-size: 13px"></span>
        </div>
        <common-table v-loading="todayShipLoading" :data="todayShipList" :height="maxRightHeight / 2" style="width: 100%">
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column prop="project.shortName" :show-overflow-tooltip="true" label="所属项目">
            <template #default="{ row }">
              <span class="project-name">{{
                projectNameFormatter(row.project, null, false)
              }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="quantity" :show-overflow-tooltip="true" label="车次" align="center">
            <template #default="{ row }">
              <span>{{ row.quantity }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="shipMete" :show-overflow-tooltip="true" label="发运量" align="center">
            <template #default="{ row }">
              <span>{{ row.shipMete }}</span>
            </template>
          </el-table-column>
        </common-table>
        <div v-loading="todayShipLoading" :style="{ height: maxRightHeight / 2 + 'px' }" style="margin-top: 15px">
          <div id="todayShipChart" style="width: 100%; height: 100%"></div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { getCargoMete, getModelList, getProjectDay, getProjectYear, getCargoQuantity, getCargoSummary } from '@/api/mes/ship-kpi'
import { onMounted, ref } from 'vue'
import moment from 'moment'

import { toFixed } from '@data-type/index'
import { projectNameFormatter } from '@/utils/project'
import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'
import panel from '@/components/Panel'

const { maxHeight: maxRightHeight } = useMaxHeight({
  extraBox: ['.head-container', '.view-right-summary', '.view-line-title'],
  extraHeight: 30
})
const { maxHeight: maxLeftHeight } = useMaxHeight({
  extraBox: ['.head-container', '.view-left-summary'],
  extraHeight: 30
})
const { maxHeight: maxCenterHeight } = useMaxHeight({ extraBox: ['.head-container', '.shipment-summary'], extraHeight: 15 })

function disabledDate(time) {
  return time > new Date()
}

const year = ref(moment().valueOf().toString())

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const summaryInfo = ref({})
const todayShipList = ref([])
const projectYearShipList = ref([])
const summaryLoading = ref(false)
const monthShipLoading = ref(false)
const carModelLoading = ref(false)
const todayShipLoading = ref(false)
const projectYearShipLoading = ref(false)

const { getMyChart: getMonthShipChart } = useChart({
  elementId: 'monthShipChart',
  fetchHook: refreshMonthShipChart,
  initOption: {
    legend: { show: false },
    xAxis: { data: monthArr.value },
    title: {
      text: '月度发运记录'
    },
    series: [
      {
        name: '车次',
        type: 'line',
        data: []
      },
      {
        name: '重量',
        type: 'line',
        data: []
      }
    ]
  }
})

const { getMyChart: getCarModelChart } = useChart({
  elementId: 'carModelChart',
  fetchHook: refreshCarModelChart,
  initOption: {
    legend: { show: false },
    xAxis: { data: [] },
    title: {
      text: '车辆信息'
    },
    series: [
      {
        name: '',
        type: 'line',
        data: []
      }
    ]
  }
})

const { getMyChart: getTodayShipChart } = useChart({
  elementId: 'todayShipChart',
  fetchHook: refreshTodayShipChart,
  initOption: {
    legend: { right: 0 },
    tooltip: {
      trigger: 'item'
    },
    series: [
      {
        name: '',
        type: 'pie',
        radius: ['30%', '60%'],
        label: {
          show: true,
          formatter: (param) => {
            const { value, name } = param.data
            return `${name}：${value}`
          }
        },
        data: []
      }
    ]
  }
})

onMounted(() => {
  refreshOtherData()
})

function refreshAll() {
  refreshOtherData()
  refreshMonthShipChart()
  refreshCarModelChart()
  refreshTodayShipChart()
}

async function refreshOtherData() {
  try {
    summaryLoading.value = true
    summaryInfo.value = {}
    const { amount, cargoListAmount, cargoListQuantity, lastDayAmount, lastDayQuantity, quantity, stock } = await getCargoSummary({
      dateTime: year.value
    })
    summaryInfo.value = {
      yearMete: (cargoListAmount && (cargoListAmount / 1000).toFixed(2)) || 0,
      yearQuantity: cargoListQuantity || 0,
      todayMete: (amount && (amount / 1000).toFixed(2)) || 0,
      todayQuantity: quantity || 0,
      lastDayMete: (lastDayAmount && (lastDayAmount / 1000).toFixed(2)) || 0,
      lastDayQuantity: lastDayQuantity || 0,
      currentStock: (stock && (stock / 1000).toFixed(2)) || 0
    }
  } catch (error) {
    console.log(error, '获取汇总信息失败')
  } finally {
    summaryLoading.value = false
  }

  try {
    projectYearShipLoading.value = true
    const { content } = await getProjectYear({ dateTime: year.value })
    projectYearShipList.value = content.map((v) => {
      v.totalWeight = (v.totalNetWeight / 1000).toFixed(2)
      v.stock = (v.stock / 1000).toFixed(2)
      v.shipWeight = (v.cargoAmount / 1000).toFixed(2)
      v.shipRate = (v.totalWeight && (v.shipWeight / v.totalWeight) * 100) || 0
      return v
    })
  } catch (error) {
    console.log(error, '获取项目发运信息失败')
  } finally {
    projectYearShipLoading.value = false
  }
}

async function refreshMonthShipChart(myChart) {
  try {
    monthShipLoading.value = true
    const _myChart = myChart || getMonthShipChart()
    const { content: quantityContent } = await getCargoQuantity({ dateTime: year.value })
    const quantityData = new Array(12).fill(0)
    for (let i = 0; i < quantityContent.length; i++) {
      const item = quantityContent[i]
      quantityData[moment(item.month).month()] = item.quantity
    }
    const { content: meteContent } = await getCargoMete({ dateTime: year.value })
    const meteData = new Array(12).fill(0)
    for (let i = 0; i < meteContent.length; i++) {
      const item = meteContent[i]
      meteData[moment(item.month).month()] = Number((item.amount / 1000).toFixed(2))
    }
    const option = _myChart.getOption()
    option.series[0].data = quantityData
    option.series[1].data = meteData
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取月度发运记录信息失败')
  } finally {
    monthShipLoading.value = false
  }
}

async function refreshCarModelChart(myChart) {
  try {
    carModelLoading.value = true
    const _myChart = myChart || getCarModelChart()
    const { content } = await getModelList({ dateTime: year.value })
    const xData = content.map((v) => v.model)
    const yData = content.map((v) => v.quantity)
    const option = _myChart.getOption()
    option.xAxis[0].data = xData
    option.series[0].data = yData
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取车辆信息失败')
  } finally {
    carModelLoading.value = false
  }
}

async function refreshTodayShipChart(myChart) {
  try {
    todayShipLoading.value = true
    const _myChart = myChart || getTodayShipChart()
    const { content } = await getProjectDay({ dateTime: moment().valueOf().toString() })
    todayShipList.value = content.map((v) => {
      v.shipMete = Number((v.cargoAmount / 1000).toFixed(2))
      v.quantity = v.cargoQuantity
      return v
    })
    const pieData = todayShipList.value.map((v) => {
      return {
        value: v.shipMete,
        name: v.project?.shortName
      }
    })
    const option = _myChart.getOption()
    option.series[0].data = pieData
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取今日发运信息失败')
  } finally {
    todayShipLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.flex-r {
  .view-left,
  .view-right {
    flex: 0.5;
  }
  .view-center {
    flex: 1;
    margin-right: 15px;
    margin-left: 15px;

    ::v-deep(.el-descriptions__content) {
      font-size: 35px;
      padding: 15px;
    }
  }
}

.target {
  ::v-deep(.el-card__header) {
    padding: 10px 20px;
  }
}

.view-line-title {
  margin-top: 15px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0px 5px;
  margin-bottom: 7px;
  font-family: 'sans-serif';
  color: #333;
  font-weight: bolder;
}
</style>
