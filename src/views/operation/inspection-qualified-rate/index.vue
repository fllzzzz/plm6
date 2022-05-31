<template>
  <div class="app-container">
    <div class="head-container">
      <el-date-picker
        v-model="year"
        type="year"
        size="small"
        style="width: 300px"
        class="filter-item"
        placeholder="选择年"
        value-format="x"
        :clearable="false"
        :disabled-date="disabledDate"
        @change="fetchChart"
      />
    </div>
    <div v-loading="loading" :gutter="20" class="panel-group">
      <panel name="检验数（件）" num-color="#1890ff" :end-val="summaryInfo.quantitySummary || 0" />
      <panel name="不合格数（件）" num-color="#1890ff" :end-val="summaryInfo.problemQuantitySummary || 0" />
      <panel name="合格率" num-color="#1890ff" :end-val="summaryInfo.avgPassRatio || 0" :precision="2" suffix="%" />
    </div>
    <el-divider class="divider" />
    <div v-loading="chartLoading" :style="{ height: maxHeight + 'px' }">
      <div id="inspectionQualifiedRateChart" style="width: 100%; height: 100%"></div>
    </div>
  </div>
</template>

<script setup>
import { getInspectionQualifiedRate } from '@/api/operation/inspection-qualified-rate'
import { ref } from 'vue'
import moment from 'moment'

import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'
import Panel from '@/components/Panel'

function disabledDate(time) {
  return time > new Date()
}

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const year = ref(moment().valueOf().toString())
const loading = ref(false)
const chartLoading = ref(false)
const summaryInfo = ref({})

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.panel-group', '.divider']
})

const { getMyChart } = useChart({
  elementId: 'inspectionQualifiedRateChart',
  fetchHook: fetchChart,
  initOption: {
    legend: { show: true },
    xAxis: { data: monthArr.value },
    title: {
      text: '月度检验合格率'
    },
    custom: {
      yAxis: [
        {
          type: 'value'
        },
        {
          name: '合格率',
          axisLabel: {
            formatter: '{value} %'
          },
          type: 'value'
        }
      ]
    },
    series: [
      {
        name: '检验数',
        type: 'bar',
        yAxisIndex: 0,
        data: []
      },
      {
        name: '不合格数',
        type: 'bar',
        yAxisIndex: 0,
        data: []
      },
      {
        name: '合格率',
        yAxisIndex: 1,
        type: 'line',
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
      quantitySummary = 0,
      avgPassRatio = 0,
      problemQuantitySummary = 0
    } = await getInspectionQualifiedRate({ dateTime: year.value })
    const option = _myChart.getOption()
    summaryInfo.value = {
      quantitySummary,
      avgPassRatio,
      problemQuantitySummary
    }
    option.series[0].data = details.map((v) => v.quantity || 0)
    option.series[1].data = details.map((v) => v.problemQuantity || 0)
    option.series[2].data = details.map((v) => v.passRatio || 0)
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取产能负荷率信息')
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
