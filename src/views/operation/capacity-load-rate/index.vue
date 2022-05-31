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
      <factory-select v-model="factoryId" class="filter-item" showOptionAll style="width: 300px" @change="fetchChart" />
    </div>
    <div v-loading="loading" :gutter="20" class="panel-group">
      <panel name="目标产量（吨）" num-color="#1890ff" :end-val="summaryInfo.targetSummary / 1000 || 0" :precision="2" />
      <panel name="累计产量（吨）" num-color="#1890ff" :end-val="summaryInfo.completeSummary / 1000 || 0" :precision="2" />
      <panel name="平均负荷率" num-color="#1890ff" :end-val="summaryInfo.avgLoadRatio || 0" :precision="2" suffix="%" />
    </div>
    <el-divider class="divider" />
    <div v-loading="chartLoading" :style="{ height: maxHeight + 'px' }">
      <div id="capacityLoadRateChart" style="width: 100%; height: 100%"></div>
    </div>
  </div>
</template>

<script setup>
import { getCapacityLoadRate } from '@/api/operation/capacity-load-rate'
import { ref } from 'vue'
import moment from 'moment'

import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'
import Panel from '@/components/Panel'
import factorySelect from '@comp-base/factory-select'

function disabledDate(time) {
  return time > new Date()
}

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const year = ref(moment().valueOf().toString())
const factoryId = ref()
const loading = ref(false)
const chartLoading = ref(false)
const summaryInfo = ref({})

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.panel-group', '.divider']
})

const { getMyChart } = useChart({
  elementId: 'capacityLoadRateChart',
  fetchHook: fetchChart,
  initOption: {
    legend: { show: true },
    xAxis: { data: monthArr.value },
    title: {
      text: '月度目标产量'
    },
    custom: {
      yAxis: [
        {
          name: '产能',
          type: 'value'
        },
        {
          name: '负荷率',
          axisLabel: {
            formatter: '{value} %'
          },
          type: 'value'
        }
      ]
    },
    series: [
      {
        name: '产能',
        type: 'line',
        yAxisIndex: 0,
        data: []
      },
      {
        name: '负荷率',
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
      completeSummary = 0,
      avgLoadRatio = 0,
      targetSummary = 0
    } = await getCapacityLoadRate({ dateTime: year.value, factoryId: factoryId.value })
    const option = _myChart.getOption()
    summaryInfo.value = {
      completeSummary,
      avgLoadRatio,
      targetSummary
    }
    option.series[0].data = details.map((v) => v.complete && Number((v.complete / 1000).toFixed(2)) || 0)
    option.series[1].data = details.map((v) => v.loadRatio || 0)
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
