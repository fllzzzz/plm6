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
        class="filter-item"
        :clearable="false"
        :disabled-date="disabledDate"
        @change="fetchInfo"
      />
      <factory-select v-model="factoryId" class="filter-item" showOptionAll style="width: 300px" @change="fetchInfo" />
      <div style="float: right; font-size: 16px; color: #303133">单位：元</div>
    </div>
    <div class="main-content">
      <div style="flex: 0.55; margin-right: 15px">
        <div v-loading="loading" :gutter="20" class="panel-group">
          <panel name="总产量（吨）" num-color="#1890ff" :end-val="summaryInfo.production || 0" :precision="2" />
          <panel name="平均辅材费（元/吨）" num-color="#1890ff" :end-val="summaryInfo.average || 0" />
        </div>
        <div :style="{ height: maxHeightChart + 'px' }">
          <div id="diffAnalysisChart" style="width: 100%; height: 100%"></div>
        </div>
      </div>
      <common-table style="flex: 1" v-loading="loading" show-summary :summary-method="getSummaries" :height="maxHeightTable" :data="list">
        <el-table-column :show-overflow-tooltip="true" label="辅材种类" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.classifyName }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="月份/消耗金额" min-width="100px" align="center">
          <el-table-column v-for="(item, index) in monthArr" :key="index" :show-overflow-tooltip="true" :label="item" align="center">
            <template #default="{ row }">
              <span>{{ (row.amount && row.amount[index + 1]) || 0 }}</span>
            </template>
          </el-table-column>
        </el-table-column>
      </common-table>
    </div>
  </div>
</template>

<script setup>
import { getAuxAnalysis } from '@/api/operation/aux-material-consumption'
import { ref } from 'vue'
import moment from 'moment'

import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'
import Panel from '@/components/Panel'
import factorySelect from '@comp-base/factory-select'

function disabledDate(time) {
  return time > new Date()
}

const year = ref(moment().valueOf().toString())
const factoryId = ref()
const loading = ref(false)
const summaryInfo = ref({})
const list = ref([])

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const { maxHeight: maxHeightChart } = useMaxHeight({
  extraBox: ['.head-container', '.panel-group']
})

const { maxHeight: maxHeightTable } = useMaxHeight({
  extraBox: ['.head-container']
})

const { getMyChart } = useChart({
  elementId: 'diffAnalysisChart',
  fetchHook: fetchInfo,
  initOption: {
    legend: { show: false },
    xAxis: { data: monthArr.value },
    title: {
      text: '辅材费用消耗'
    },
    series: [
      {
        name: '费用消耗',
        type: 'bar',
        data: [],
        markPoint: {
          data: [
            { type: 'max', name: 'Max' },
            { type: 'min', name: 'Min' }
          ]
        }
      }
    ]
  }
})

// 合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    sums[index] = data.reduce((sum, v) => {
      sum += (v.amount && v.amount[index]) || 0
      return sum
    }, 0)
  })
  return sums
}

async function fetchInfo() {
  try {
    summaryInfo.value = {}
    const _myChart = getMyChart()
    const { outboundAmounts, productionAnalysisDetails } = await getAuxAnalysis({ dateTime: year.value, factoryId: factoryId.value })
    list.value = outboundAmounts
    const sumProduction = productionAnalysisDetails.reduce((sum, v) => {
      sum += v.production
      return sum
    }, 0)
    summaryInfo.value.production = sumProduction / 1000
    const chartAmountData = new Array(12).fill(0)
    for (let i = 0; i < chartAmountData.length; i++) {
      for (let x = 0; x < outboundAmounts.length; x++) {
        if (outboundAmounts[x].amount && outboundAmounts[x].amount[i + 1]) {
          outboundAmounts[x].amount[i + 1] = Number(outboundAmounts[x].amount[i + 1].toFixed(0))
          chartAmountData[i] += outboundAmounts[x].amount[i + 1] || 0
        }
      }
    }
    const sumAmount = chartAmountData.reduce((sum, v) => {
      sum += v
      return sum
    }, 0)
    // summaryInfo.value.amount = sumAmount
    summaryInfo.value.average = summaryInfo.value.production ? sumAmount / summaryInfo.value.production : 0
    const option = _myChart.getOption()
    option.series[0].data = chartAmountData
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取辅材消耗失败')
  }
}
</script>

<style lang="scss" scoped>
.panel-group {
  display: flex;
  justify-content: space-between;
  margin-bottom: 20px;

  & > * {
    flex: 1;
  }

  & > *:not(:last-child) {
    margin-right: 10px;
  }
}

.main-content {
  display: flex;
  justify-content: space-between;
}
</style>
