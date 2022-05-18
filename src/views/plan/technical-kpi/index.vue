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
      <div style="float: right; font-size: 16px; color: #303133">单位：吨</div>
    </div>
    <div class="flex-r">
      <div class="view-left">
        <div class="view-left-summary panel-group">
          <panel name="年度深化量" num-color="#1890ff" :end-val="yearMeteSummary || 0" :precision="2" />
        </div>
        <div class="flex-c">
          <div v-loading="monthCompleteLoading" :style="{ height: maxLeftHeight / 3 + 'px' }">
            <div id="monthCompleteChart" style="width: 100%; height: 100%"></div>
          </div>
          <div
            v-loading="projectCompleteLoading"
            :style="{ height: maxLeftHeight / 3 + 'px' }"
            style="margin-top: 15px; position: relative"
          >
            <el-date-picker
              v-model="projectMonth"
              type="month"
              size="mini"
              style="width: 120px; position: absolute; right: 0; z-index: 99"
              placeholder="选择月"
              value-format="x"
              :disabled-date="disabledDate"
              @change="refreshProjectCompleteChart"
            />
            <div id="projectCompleteChart" style="width: 100%; height: 100%"></div>
          </div>
          <common-table v-loading="projectCompleteLoading" :data="projectCompleteList" :height="maxLeftHeight / 3" style="width: 100%">
            <!-- <el-table-column label="序号" type="index" align="center" width="50" /> -->
            <el-table-column prop="color" :show-overflow-tooltip="true" align="center" width="50">
              <template #default="{ row }">
                <span
                  class="color-card"
                  :style="{ 'background-color': row.chartActive ? row.color : '#ccc' }"
                  @click="handleProjectCompleteClick(row)"
                />
              </template>
            </el-table-column>
            <el-table-column prop="project.shortName" :show-overflow-tooltip="true" label="所属项目" min-width="120">
              <template #default="{ row }">
                <span class="project-name">{{ projectNameFormatter(row.project, null, false) }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="mete" :show-overflow-tooltip="true" label="深化量" align="center" width="100">
              <template #default="{ row }">
                <span>{{ row.mete }}</span>
              </template>
            </el-table-column>
          </common-table>
        </div>
      </div>
      <div class="view-center">
        <el-descriptions direction="vertical" :column="4" border class="technical-summary">
          <el-descriptions-item align="center" label="项目总数">{{ summaryInfo.sumQuantity || 0 }}</el-descriptions-item>
          <el-descriptions-item align="center" label="已完工">{{ summaryInfo.completedQuantity || 0 }}</el-descriptions-item>
          <el-descriptions-item align="center" label="进行中">{{ summaryInfo.processingQuantity || 0 }}</el-descriptions-item>
          <el-descriptions-item align="center" label="本月新增">{{ summaryInfo.monthNewQuantity || 0 }}</el-descriptions-item>
        </el-descriptions>
        <common-table
          v-loading="projectYearSummaryLoading"
          :data="projectYearSummaryList"
          :height="maxCenterHeight"
          style="width: 100%; margin-top: 15px"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column prop="project.shortName" :show-overflow-tooltip="true" label="所属项目">
            <template #default="{ row }">
              <span class="project-name">{{ projectNameFormatter(row.project, null, false) }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="planMete" :show-overflow-tooltip="true" label="合同量" align="center" width="100">
            <template #default="{ row }">
              <span>{{ row.planMete }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="mete" :show-overflow-tooltip="true" label="已完成" align="center">
            <template #default="{ row }">
              <el-progress :text-inside="true" :stroke-width="26" :percentage="row.ratio">
                <template #default="{ percentage }">
                  <span>{{ row.mete }}</span> | <span>{{ toFixed(percentage, 2) }}%</span>
                </template>
              </el-progress>
            </template>
          </el-table-column>
        </common-table>
      </div>
      <div class="view-right">
        <div v-loading="userYearMeteLoading" :style="{ height: maxRightHeight / 3 + 'px' }">
          <div id="userYearMeteChart" style="width: 100%; height: 100%"></div>
        </div>
        <div v-loading="userMonthMeteLoading" :style="{ height: maxRightHeight / 3 + 'px' }" style="margin-top: 15px; position: relative">
          <el-date-picker
            v-model="userMonth"
            type="month"
            size="mini"
            style="width: 120px; position: absolute; right: 0; z-index: 99"
            placeholder="选择月"
            value-format="x"
            :disabled-date="disabledDate"
            @change="refreshUserMonthMeteChart"
          />
          <div id="userMonthMeteChart" style="width: 100%; height: 100%"></div>
        </div>
        <common-table
          v-loading="userMonthMeteLoading"
          :data="userMonthMeteList"
          :height="maxRightHeight / 3"
          style="width: 100%; margin-top: 15px"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column prop="userName" :show-overflow-tooltip="true" label="姓名" align="center">
            <template #default="{ row }">
              <span>{{ row.userName }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="mete" :show-overflow-tooltip="true" label="上月" align="center">
            <template #default="{ row }">
              <span>{{ row.mete }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="lastMonthMete" :show-overflow-tooltip="true" label="本月" align="center">
            <template #default="{ row }">
              <span>{{ row.lastMonthMete }}</span>
            </template>
          </el-table-column>
        </common-table>
      </div>
    </div>
  </div>
</template>

<script setup>
import {
  getProjectSummary,
  getMonthMete,
  getProjectMete,
  getUserYearMete,
  getUserMonthMete,
  getProjectYearSummary
} from '@/api/plan/technical-kpi'
import { onMounted, ref } from 'vue'
import moment from 'moment'

import { toFixed } from '@data-type/index'
import { projectNameFormatter } from '@/utils/project'
import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'
import panel from '@/components/Panel'

const { maxHeight: maxRightHeight } = useMaxHeight({ extraHeight: 30 })
const { maxHeight: maxLeftHeight } = useMaxHeight({
  extraBox: ['.head-container', '.view-left-summary'],
  extraHeight: 15
})
const { maxHeight: maxCenterHeight } = useMaxHeight({ extraBox: ['.head-container', '.technical-summary'], extraHeight: 15 })

function disabledDate(time) {
  return time > new Date()
}

const chartsDefaultColors = ['#5470c6', '#91cc75', '#fac858', '#ee6666', '#73c0de', '#3ba272', '#fc8452', '#9a60b4', '#ea7ccc']

const year = ref(moment().valueOf().toString())
const projectMonth = ref(moment().valueOf().toString())
const userMonth = ref(moment().valueOf().toString())

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const summaryInfo = ref({})
const yearMeteSummary = ref(0)
const projectYearSummaryList = ref([])
const projectCompleteList = ref([])
const userMonthMeteList = ref([])
const summaryLoading = ref(false)
const projectYearSummaryLoading = ref(false)
const monthCompleteLoading = ref(false)
const projectCompleteLoading = ref(false)
const userYearMeteLoading = ref(false)
const userMonthMeteLoading = ref(false)

const { getMyChart: getMonthCompleteChart } = useChart({
  elementId: 'monthCompleteChart',
  fetchHook: refreshMonthCompleteChart,
  initOption: {
    legend: { show: false },
    xAxis: { data: monthArr.value },
    title: {
      text: '月度完成量统计'
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

const { getMyChart: getProjectCompleteChart } = useChart({
  elementId: 'projectCompleteChart',
  fetchHook: refreshProjectCompleteChart,
  initOption: {
    legend: { show: false },
    xAxis: { show: false },
    tooltip: {
      trigger: 'item'
    },
    title: {
      text: '项目完成量分析'
    },
    series: [
      {
        name: '',
        type: 'pie',
        radius: ['40%', '70%'],
        label: {
          show: true,
          position: 'inner',
          formatter: '{d}%'
        },
        top: 15,
        data: []
      }
    ]
  }
})

const { getMyChart: getUserYearMeteChart } = useChart({
  elementId: 'userYearMeteChart',
  fetchHook: refreshUserYearMeteChart,
  initOption: {
    legend: { show: false },
    xAxis: { data: [] },
    title: {
      text: '员工年度深化量统计'
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

const { getMyChart: getUserMonthMeteChart } = useChart({
  elementId: 'userMonthMeteChart',
  fetchHook: refreshUserMonthMeteChart,
  initOption: {
    legend: { show: false },
    xAxis: { data: [] },
    title: {
      text: '员工月度深化量统计'
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

onMounted(() => {
  refreshOtherData()
})

function handleYearChange() {
  projectMonth.value = moment(Number(year.value)).set('month', 0)
  userMonth.value = moment(Number(year.value)).set('month', 0)
  // if (moment(Number(year.value)).year() === moment().year()) {
  // }
}

function refreshAll() {
  handleYearChange()
  refreshOtherData()
  refreshMonthCompleteChart()
  refreshProjectCompleteChart()
  refreshUserYearMeteChart()
  refreshUserMonthMeteChart()
}

async function refreshOtherData() {
  try {
    summaryLoading.value = true
    summaryInfo.value = {}
    const { monthNewQuantity, processingQuantity, completedQuantity, sumQuantity } = await getProjectSummary({
      year: moment(Number(year.value)).year()
    })
    summaryInfo.value = {
      monthNewQuantity,
      processingQuantity,
      completedQuantity,
      sumQuantity
    }
  } catch (error) {
    console.log(error, '获取汇总信息失败')
  } finally {
    summaryLoading.value = false
  }

  try {
    projectYearSummaryLoading.value = true
    const { content } = await getProjectYearSummary({ year: moment(Number(year.value)).year() })
    projectYearSummaryList.value = content.map((v) => {
      v.project = { ...v }
      v.mete = Number(v.mete.toFixed(2)) || 0
      v.planMete = Number(v.planMete.toFixed(2)) || 0
      v.ratio = (v.mete / v.planMete) * 100
      return v
    })
  } catch (error) {
    console.log(error, '获取项目汇总信息失败')
  } finally {
    projectYearSummaryLoading.value = false
  }
}

async function refreshMonthCompleteChart() {
  try {
    monthCompleteLoading.value = true
    yearMeteSummary.value = 0
    const _myChart = getMonthCompleteChart()
    const { content } = await getMonthMete({ year: moment(Number(year.value)).year() })
    const meteData = new Array(12).fill(0)

    for (let i = 0; i < content.length; i++) {
      const item = content[i]
      meteData[item.month - 1] = Number(item.mete.toFixed(2))
      yearMeteSummary.value += item.mete
    }
    const option = _myChart.getOption()
    option.series[0].data = meteData
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取月度深化量信息失败')
  } finally {
    monthCompleteLoading.value = false
    yearMeteSummary.value = Number(yearMeteSummary.value.toFixed(2))
  }
}

async function refreshProjectCompleteChart() {
  try {
    projectCompleteLoading.value = true
    const _myChart = getProjectCompleteChart()
    const { content } = await getProjectMete({
      year: moment(Number(projectMonth.value)).year(),
      month: moment(Number(projectMonth.value)).month() + 1
    })
    projectCompleteList.value = content.map((v, i) => {
      v.project = { ...v }
      v.mete = Number(v.mete.toFixed(2))
      v.color = chartsDefaultColors[i % chartsDefaultColors.length]
      v.chartActive = true
      return v
    })
    const option = _myChart.getOption()

    option.series[0].data = projectCompleteList.value.map((v) => {
      return {
        value: v.mete,
        name: v.shortName
      }
    })
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取每个项目深化量信息失败')
  } finally {
    projectCompleteLoading.value = false
  }
}

function handleProjectCompleteClick(row) {
  row.chartActive = !row.chartActive
  const _myChart = getProjectCompleteChart()
  _myChart.dispatchAction({
    type: 'legendToggleSelect',
    // 图例名称
    name: row.shortName
  })
}

async function refreshUserYearMeteChart() {
  try {
    userYearMeteLoading.value = true
    const _myChart = getUserYearMeteChart()
    const { content } = await getUserYearMete({ year: moment(Number(year.value)).year() })

    const option = _myChart.getOption()
    option.xAxis[0].data = content.map((v) => v.userName)
    option.series[0].data = content.map((v) => Number(v.mete.toFixed(2)))
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取员工年度深化量信息失败')
  } finally {
    userYearMeteLoading.value = false
  }
}

async function refreshUserMonthMeteChart() {
  try {
    userMonthMeteLoading.value = true
    userMonthMeteList.value = []
    const _myChart = getUserMonthMeteChart()
    const { content } = await getUserMonthMete({
      year: moment(Number(userMonth.value)).year(),
      month: moment(Number(userMonth.value)).month() + 1
    })
    userMonthMeteList.value = content.map((v) => {
      v.mete = Number(v.mete.toFixed(2))
      v.lastMonthMete = Number(v.lastMonthMete.toFixed(2))
      return v
    })
    const option = _myChart.getOption()
    option.xAxis[0].data = userMonthMeteList.value.map((v) => v.userName)
    option.series[0].data = userMonthMeteList.value.map((v) => v.mete)
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取员工月度深化量信息失败')
  } finally {
    userMonthMeteLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.color-card {
  display: inline-block;
  width: 20px;
  border-radius: 3px;
  height: 15px;
  cursor: pointer;
  vertical-align: middle;
}

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

.panel-group {
  margin-bottom: 10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        text-align: left;
        margin-top: 2px;
        color: #333;
      }
      .card-panel-num {
        display: block;
        font-size: 20px;
        text-align: right;
      }
    }
  }
}
</style>
