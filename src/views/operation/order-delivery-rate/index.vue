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
      <panel name="在手订单（吨）" num-color="#1890ff" :end-val="summaryInfo.orderSummary / 1000 || 0" :precision="2" />
      <panel name="实际交付（吨）" num-color="#1890ff" :end-val="summaryInfo.completeSummary / 1000 || 0" :precision="2" />
      <panel name="总交付率" num-color="#1890ff" :end-val="summaryInfo.totalRatio|| 0" :precision="2" suffix="%"/>
      <panel name="平均交付率" num-color="#1890ff" :end-val="summaryInfo.avgDeliverRatio || 0" :precision="2" suffix="%" />
    </div>
    <el-divider class="divider" />
    <div v-loading="chartLoading" :style="{ height: maxHeight + 'px' }">
      <div id="orderDeliveryRateChart" style="width: 100%; height: 100%"></div>
    </div>
  </div>
</template>

<script setup>
import { getOrderDeliveryRate } from '@/api/operation/order-delivery-rate'
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
  elementId: 'orderDeliveryRateChart',
  fetchHook: fetchChart,
  initOption: {
    legend: { show: true },
    xAxis: { data: monthArr.value },
    title: {
      text: '月度订单交付率'
    },
    custom: {
      yAxis: [
        {
          type: 'value'
        },
        {
          name: '交付率',
          axisLabel: {
            formatter: '{value} %'
          },
          type: 'value'
        }
      ]
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
      },
      {
        name: '交付率',
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
    option.series[0].data = details.map((v) => v.order && Number((v.order / 1000).toFixed(2)) || 0)
    option.series[1].data = details.map((v) => {
      const value = v.complete && Number((v.complete / 1000).toFixed(2)) || 0
      return {
        value,
        itemStyle: {
          color: value && !v.order ? '#cccccc' : '#91cc75'
        }
      }
    })
    option.series[2].data = details.map((v) => v.deliverRatio || 0)
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
