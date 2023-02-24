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
        @change="fetchList"
      />
      <div style="float: right; font-size: 16px; color: #303133">单位：吨</div>
    </div>
    <div v-loading="loading" :gutter="20" class="panel-group">
      <panel name="项目数" num-color="#1890ff" :end-val="summaryInfo.projectQuantity || 0" />
      <panel name="生产总量" num-color="#1890ff" :end-val="summaryInfo.production / 1000 || 0" />
      <panel name="钢材使用量" num-color="#1890ff" :end-val="summaryInfo.usage / 1000 || 0" />
      <panel name="差异" num-color="#f56c6c" :end-val="summaryInfo.diff / 1000 || 0" />
      <panel name="差异率" num-color="#f56c6c" :end-val="summaryInfo.diffRatio || 0" :suffix="'%'" />
    </div>
    <common-table v-loading="loading" :height="maxHeight / 2" :data="list">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns showProject />
      <el-table-column :show-overflow-tooltip="true" label="制成品总量" min-width="100px" align="center">
        <template #default="{ row }">
          <span>{{ (row.production / 1000).toFixed(2) }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" label="钢材使用量" min-width="100px" align="center">
        <template #default="{ row }">
          <span>{{ (row.amountOfUsage / 1000).toFixed(2) }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" label="差异" min-width="100px" align="center">
        <template #default="{ row }">
          <span>{{ (row.difference / 1000).toFixed(2) }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" label="差异率" min-width="100px" align="center">
        <template #default="{ row }">
          <span>{{ row.differenceRatio || 0 }}%</span>
        </template>
      </el-table-column>
    </common-table>
    <el-divider class="divider" />
    <div v-loading="chartLoading" :style="{ height: maxHeight / 2 + 'px' }">
      <div id="diffAnalysisChart" style="width: 100%; height: 100%"></div>
    </div>
  </div>
</template>

<script setup>
import { getDiffAnalysis, getDiffAverageAnalysis } from '@/api/operation/difference-analysis'
import { onMounted, ref } from 'vue'
import moment from 'moment'

import checkPermission from '@/utils/system/check-permission'
import { operationDifferenceAnalysisPM as permission } from '@/page-permission/operation'
import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import Panel from '@/components/Panel'

function disabledDate(time) {
  return time > new Date()
}

const year = ref(moment().valueOf().toString())
const loading = ref(false)
const chartLoading = ref(false)
const summaryInfo = ref({})
const list = ref([])

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.panel-group', '.divider']
})

const { getMyChart } = useChart({
  elementId: 'diffAnalysisChart',
  fetchHook: fetchChart,
  initOption: {
    legend: { show: false },
    xAxis: { data: [] },
    yAxis: {
      axisLabel: {
        formatter: '{value} %'
      }
    },
    title: {
      text: '近十年平均差异率'
    },
    series: [
      {
        name: '',
        markPoint: {
          data: [
            { type: 'max', name: 'Max' },
            { type: 'min', name: 'Min' }
          ]
        },
        type: 'line',
        data: []
      }
    ]
  }
})

onMounted(() => {
  fetchList()
})

async function fetchList() {
  if (!checkPermission(permission.get)) {
    return false
  }
  try {
    loading.value = true
    const { content } = await getDiffAnalysis({ dateTime: year.value })
    summaryInfo.value = content.reduce(
      (obj, data) => {
        obj.projectQuantity++
        obj.production += data.production
        obj.usage += data.amountOfUsage
        return obj
      },
      {
        projectQuantity: 0,
        production: 0,
        usage: 0
      }
    )
    summaryInfo.value.diff = summaryInfo.value.usage && Math.abs(summaryInfo.value.usage - summaryInfo.value.production) || 0
    summaryInfo.value.diffRatio = summaryInfo.value.usage && (summaryInfo.value.diff / summaryInfo.value.usage) * 100
    list.value = content
  } catch (error) {
    console.log(error, '获取差异分析列表失败')
  } finally {
    loading.value = false
  }
}

async function fetchChart(myChart) {
  if (!checkPermission(permission.get)) {
    return false
  }
  try {
    chartLoading.value = true
    const _myChart = myChart || getMyChart()
    const { content } = await getDiffAverageAnalysis({ range: 10 })
    const option = _myChart.getOption()
    option.xAxis[0].data = content.map((v) => v.year)
    option.series[0].data = content.map((v) => v.averageDifferenceRatio)
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取差异分析柱状图失败')
  } finally {
    chartLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.panel-group {
  display: flex;
  justify-content: space-between;
  margin-bottom: 10px;

  & > * {
    flex: 1;
  }

  & > *:not(:last-child) {
    margin-right: 10px;
  }
}
</style>
