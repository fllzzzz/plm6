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
        :clearable="false"
        :disabled-date="disabledDate"
        @change="refreshAll"
      />
    </div>
    <div class="flex-r">
      <div class="view-left">
        <el-card v-loading="targetLoading" class="target" style="margin-bottom: 15px">
          <template #header>
            <span>目标达成</span>
          </template>
          <div class="flex-r">
            <div class="flex-rcc" style="flex: 1">
              <el-progress
                type="circle"
                :percentage="Number(targetInfo.completeRate || 0)"
                :stroke-width="16"
                :width="140"
                :color="colors"
              >
                <!-- <template #default="{ percentage }"> -->
                <!-- <span style="font-size: 16px">{{ toFixed(percentage, 2) }}%</span> -->
                <!-- </template> -->
              </el-progress>
            </div>
            <div style="flex: 1; padding: 0px 10px" class="flex-ccc" border-style="dashed">
              <div>
                <div style="margin-bottom: 5px">目标产量</div>
                <div style="font-size: 20px; font-weight: bold; text-align: center">{{ targetInfo.target || 0 }}</div>
              </div>
              <el-divider style="margin: 20px 0"></el-divider>
              <div>
                <div style="margin-bottom: 5px">实时产量</div>
                <div style="font-size: 20px; font-weight: bold; text-align: center">{{ targetInfo.completeMete || 0 }}</div>
              </div>
            </div>
          </div>
        </el-card>
        <div class="flex-c">
          <div v-loading="monthYieldLoading" :style="{ height: maxLeftHeight / 2 + 'px' }">
            <div id="monthYieldChart" style="width: 100%; height: 100%"></div>
          </div>
          <div class="view-line-title">
            <span style="font-size: 18px">生产线产量</span>
            <span style="font-size: 13px">单位：吨</span>
          </div>
          <common-table v-loading="lineYieldLoading" :data="lineYieldList" :height="maxLeftHeight / 2" style="width: 100%">
            <el-table-column label="序号" type="index" align="center" width="50" />
            <el-table-column prop="workshopName" :show-overflow-tooltip="true" label="车间/生产线" align="center" min-width="140">
              <template #default="{ row }">
                <span>{{ row.workshopName }}>{{ row.name }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="lastMonthYield" :show-overflow-tooltip="true" label="上月" align="center">
              <template #default="{ row }">
                <span>{{ row.lastMonthYield }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="yield" :show-overflow-tooltip="true" label="本月" align="center">
              <template #default="{ row }">
                <span>{{ row.yield }}</span>
              </template>
            </el-table-column>
          </common-table>
        </div>
      </div>
      <div class="view-center">
        <el-descriptions v-loading="summaryLoading" direction="vertical" :column="6" border class="project-summary">
          <el-descriptions-item align="center" label="全部项目">{{ summaryInfo.sumQuantity || 0 }}</el-descriptions-item>
          <el-descriptions-item align="center" label="加工订单">{{ summaryInfo.processQuantity || 0 }}</el-descriptions-item>
          <el-descriptions-item align="center" label="海外订单">{{ summaryInfo.overseaQuantity || 0 }}</el-descriptions-item>
          <el-descriptions-item align="center" label="承包订单">{{ summaryInfo.jobQuantity || 0 }}</el-descriptions-item>
          <el-descriptions-item align="center" label="进行中">{{ summaryInfo.processingQuantity || 0 }}</el-descriptions-item>
          <el-descriptions-item align="center" label="完工">{{ summaryInfo.completedQuantity || 0 }}</el-descriptions-item>
        </el-descriptions>
        <common-table
          v-loading="projectYieldLoading"
          :data="projectYieldList"
          :height="maxCenterHeight"
          style="width: 100%; margin-top: 15px"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column prop="project.shortName" :show-overflow-tooltip="true" label="所属项目">
            <template #default="{ row }">
              <span class="project-name">{{ projectNameFormatter(row.project, null, false) }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="totalWeight" :show-overflow-tooltip="true" label="清单量" align="center" width="100">
            <template #default="{ row }">
              <span>{{ row.totalWeight }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="productWeight" :show-overflow-tooltip="true" label="已生产" align="center">
            <template #default="{ row }">
              <el-progress :text-inside="true" :stroke-width="26" :percentage="row.productRate">
                <span>{{ row.productWeight }}</span>
              </el-progress>
            </template>
          </el-table-column>
          <el-table-column prop="shipWeight" :show-overflow-tooltip="true" label="已发运" align="center">
            <template #default="{ row }">
              <el-progress :text-inside="true" :stroke-width="26" :percentage="row.shipRate" status="success">
                <span>{{ row.shipWeight }}</span>
              </el-progress>
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div class="view-right">
        <div v-loading="shipYieldLoading" :style="{ height: maxHeight / 3 + 'px' }" style="margin-bottom: 15px">
          <div id="shipmentChart" style="width: 100%; height: 100%"></div>
        </div>
        <div v-loading="qhseLoading" :style="{ height: maxHeight / 3 + 'px' }" style="margin-bottom: 15px">
          <div id="qhsePieChart" style="width: 100%; height: 100%"></div>
        </div>
        <div v-loading="qhseQualityLoading" :style="{ height: maxHeight / 3 + 'px' }">
          <div id="qhseQualityChart" style="width: 100%; height: 100%"></div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import {
  getProjectSummary,
  getTargetComplete,
  getCargoList,
  getYieldList,
  getLineYieldList,
  getProjectYieldList,
  getQHSEList,
  getQHSEQualityList
} from '@/api/mes/production-kpi'
import { onMounted, ref } from 'vue'
import moment from 'moment'

import { projectNameFormatter } from '@/utils/project'
import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'

const { maxHeight } = useMaxHeight({ extraHeight: 30, minHeight: 600 })
const { maxHeight: maxLeftHeight } = useMaxHeight({ extraBox: ['.head-container', '.target', '.view-line-title'], extraHeight: 15 })
const { maxHeight: maxCenterHeight } = useMaxHeight({ extraBox: ['.head-container', '.project-summary'], extraHeight: 15 })

function disabledDate(time) {
  return time > new Date()
}

const year = ref(moment().valueOf().toString())

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const colors = [
  { color: '#f56c6c', percentage: 30 },
  { color: '#e6a23c', percentage: 70 },
  { color: '#6f7ad3', percentage: 100 }
]

const targetInfo = ref({})
const summaryInfo = ref({})
const lineYieldList = ref([])
const projectYieldList = ref([])
const targetLoading = ref(false)
const summaryLoading = ref(false)
const monthYieldLoading = ref(false)
const shipYieldLoading = ref(false)
const qhseLoading = ref(false)
const qhseQualityLoading = ref(false)
const lineYieldLoading = ref(false)
const projectYieldLoading = ref(false)

const { getMyChart: getMonthYieldChart } = useChart({
  elementId: 'monthYieldChart',
  fetchHook: refreshMonthYieldChart,
  initOption: {
    legend: { show: false },
    xAxis: { data: monthArr.value },
    custom: {
      title: [
        {
          text: '月产量分析'
        },
        { text: '单位：吨', right: 0, textStyle: { fontSize: 13 }}
      ]
    },
    series: [
      {
        name: '',
        type: 'bar',
        data: []
      }
    ]
  }
})

const { getMyChart: getShipmentChart } = useChart({
  elementId: 'shipmentChart',
  fetchHook: refreshShipmentChart,
  initOption: {
    legend: { show: false },
    xAxis: { data: monthArr.value },
    title: {
      text: '发运车次'
    },
    series: [
      {
        name: '',
        type: 'bar',
        data: []
      }
    ]
  }
})

const { getMyChart: getQHSEChart } = useChart({
  elementId: 'qhsePieChart',
  fetchHook: refreshQHSEChart,
  initOption: {
    legend: { right: 0 },
    title: {
      text: 'QHSE事件'
    },
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

const { getMyChart: getQHSEQualityChart } = useChart({
  elementId: 'qhseQualityChart',
  fetchHook: refreshQHSEQualityChart,
  initOption: {
    legend: { show: false },
    xAxis: { data: [] },
    title: {
      text: '质量事件分析'
    },
    series: [
      {
        name: '',
        barWidth: '25px',
        type: 'bar',
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
  refreshMonthYieldChart()
  refreshShipmentChart()
  refreshQHSEChart()
  refreshQHSEQualityChart()
}

async function refreshOtherData() {
  try {
    summaryLoading.value = true
    const data = await getProjectSummary({ year: moment(Number(year.value)).year() })
    summaryInfo.value = data
  } catch (error) {
    console.log(error, '获取项目数量统计失败')
  } finally {
    summaryLoading.value = false
  }
  try {
    targetLoading.value = true
    const data = await getTargetComplete({ dateTime: year.value })
    data.completeMete = data.completeMete && (data.completeMete / 1000).toFixed(2) || 0
    data.target = data.target && (data.target / 1000).toFixed(2) || 0
    data.completeRate = data.target && Number(((data.completeMete / data.target) * 100)) || 0
    targetInfo.value = data
  } catch (error) {
    console.log(error, '获取目标达成失败')
  } finally {
    targetLoading.value = false
  }
  try {
    lineYieldLoading.value = true
    const { content } = await getLineYieldList({ dateTime: year.value })
    lineYieldList.value = content.map((v) => {
      v.lastMonthYield = (v.lastMonthYield / 1000).toFixed(0)
      v.yield = (v.yield / 1000).toFixed(0)
      return v
    })
  } catch (error) {
    console.log(error, '获取生产线产量信息失败')
  } finally {
    lineYieldLoading.value = false
  }
  try {
    projectYieldLoading.value = true
    const { content } = await getProjectYieldList({ dateTime: year.value })
    projectYieldList.value = content.map((v) => {
      v.totalWeight = (v.totalNetWeight / 1000).toFixed(0)
      v.productWeight = (v.yield / 1000).toFixed(0)
      v.productRate = (v.totalWeight && (v.productWeight / v.totalWeight) * 100) || 0
      v.shipWeight = (v.despatchAmount / 1000).toFixed(0)
      v.shipRate = (v.totalWeight && (v.shipWeight / v.totalWeight) * 100) || 0
      return v
    })
  } catch (error) {
    console.log(error, '获取项目产量信息失败')
  } finally {
    projectYieldLoading.value = false
  }
}

async function refreshMonthYieldChart(myChart) {
  try {
    monthYieldLoading.value = true
    const _myChart = myChart || getMonthYieldChart()
    const { content } = await getYieldList({ dateTime: year.value })
    const monthData = new Array(12).fill(0)
    for (let i = 0; i < content.length; i++) {
      const item = content[i]
      monthData[moment(item.month).month()] = Number((item.yield / 1000).toFixed(0))
    }
    const option = _myChart.getOption()
    option.series[0].data = monthData
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取每月产量信息失败')
  } finally {
    monthYieldLoading.value = false
  }
}

async function refreshShipmentChart(myChart) {
  try {
    shipYieldLoading.value = true
    const _myChart = myChart || getShipmentChart()
    const { content } = await getCargoList({ dateTime: year.value })
    const monthData = new Array(12).fill(0)
    for (let i = 0; i < content.length; i++) {
      const item = content[i]
      monthData[moment(item.month).month()] = item.quantity
    }
    const option = _myChart.getOption()
    option.series[0].data = monthData
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取发运车次信息失败')
  } finally {
    shipYieldLoading.value = false
  }
}

async function refreshQHSEChart(myChart) {
  try {
    qhseLoading.value = true
    const _myChart = myChart || getQHSEChart()
    const { content } = await getQHSEList({ dateTime: year.value })
    const pieData = content.map((v) => {
      return {
        value: v.quantity,
        name: v.name
      }
    })
    const option = _myChart.getOption()
    option.series[0].data = pieData
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取QHSE信息失败')
  } finally {
    qhseLoading.value = false
  }
}

async function refreshQHSEQualityChart(myChart) {
  try {
    qhseQualityLoading.value = true
    const _myChart = myChart || getQHSEQualityChart()
    const { content } = await getQHSEQualityList({ dateTime: year.value })
    const xData = content.map((v) => v.type)
    const yData = content.map((v) => v.quantity)
    const option = _myChart.getOption()
    option.xAxis[0].data = xData
    option.series[0].data = yData
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取质量事件信息失败')
  } finally {
    qhseQualityLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.flex-r {
  .view-left,
  .view-right {
    flex: 0.5;
    min-width: 350px;
  }
  .view-center {
    flex: 1;
    min-width: 600px;
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
