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
        @change="fetchInfo"
      />
    </div>
    <div v-loading="loading" class="panel-group">
      <panel
        v-for="(item, key) in typeEnum.ENUM"
        :key="key"
        :name="typeEnum.VL[item.V]"
        num-color="#1890ff"
        :end-val="summaryInfo[item.V] || 0"
      />
      <panel name="总计" num-color="#1890ff" :end-val="totalQuantity || 0" />
    </div>
    <div style="display: flex">
      <div v-loading="loading" :style="{ height: maxHeight + 'px' }" style="flex: 1; margin-right: 20px">
        <div id="qhseAnalysisRingChart" style="width: 100%; height: 100%"></div>
      </div>
      <div v-loading="loading" :style="{ height: maxHeight + 'px' }" style="flex: 1">
        <div id="qhseAnalysisChart" style="width: 100%; height: 100%"></div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { getQHSEAnalysis } from '@/api/operation/QHSE'
import { ref } from 'vue'
import moment from 'moment'

import { isBlank } from '@data-type/index'
import { constantize } from '@/utils/enum/base'
import checkPermission from '@/utils/system/check-permission'
import { QHSEAnalysisPM as permission } from '@/page-permission/operation'

import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'
import Panel from '@/components/Panel'

const typeEnum = {
  QUALITY: { L: '质量事件', K: 'QUALITY', V: 1 << 0 },
  SAFETY: { L: '安全事件', K: 'SAFETY', V: 1 << 1 },
  ENVIRONMENT: { L: '环境事件', K: 'ENVIRONMENT', V: 1 << 2 }
}
constantize(typeEnum)

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const summaryInfo = ref({})
const typeData = ref({})
const totalQuantity = ref(0)

function disabledDate(time) {
  return time > new Date()
}

const year = ref(moment().valueOf().toString())
const loading = ref(false)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.panel-group']
})

const { getMyChart: getMyRingChart } = useChart({
  elementId: 'qhseAnalysisRingChart',
  initOption: {
    tooltip: {
      trigger: 'item'
    },
    series: [
      {
        type: 'pie',
        radius: ['40%', '70%'],
        label: {
          show: true
        },
        data: []
      }
    ]
  }
})

const { getMyChart: getMyLineChart } = useChart({
  elementId: 'qhseAnalysisChart',
  fetchHook: fetchInfo,
  initOption: {
    xAxis: { data: monthArr.value },
    series: []
  }
})

function init() {
  summaryInfo.value = {}
  typeData.value = {}
  totalQuantity.value = 0
}

async function fetchInfo() {
  try {
    init()
    if (!checkPermission(permission.get)) {
      return false
    }
    loading.value = true
    const { content } = await getQHSEAnalysis({ dateTime: year.value })
    for (let i = 0; i < content.length; i++) {
      const typeItem = content[i]
      if (isBlank(summaryInfo.value[typeItem.type])) summaryInfo.value[typeItem.type] = 0
      if (isBlank(typeData.value[typeItem.type])) typeData.value[typeItem.type] = new Array(12).fill(0)
      for (let x = 0; x < typeItem.qhseDetailsList.length; x++) {
        const item = typeItem.qhseDetailsList[x]
        summaryInfo.value[typeItem.type] += item.quantity
        totalQuantity.value += item.quantity
        typeData.value[typeItem.type][moment(item.month).month()] = item.quantity
      }
    }

    setRingChart()
    setLineChart()
  } catch (error) {
    console.log(error, '获取QHSE信息')
  } finally {
    loading.value = false
  }
}

function setRingChart() {
  const seriesData = []
  for (const item in summaryInfo.value) {
    seriesData.push({
      value: summaryInfo.value[item],
      name: typeEnum.VL[item]
    })
  }
  const _myChart = getMyRingChart()
  const option = _myChart.getOption()
  option.series[0].data = seriesData
  _myChart.setOption(option)
}

function setLineChart() {
  const seriesData = []
  for (const item in typeData.value) {
    seriesData.push({
      type: 'line',
      data: typeData.value[item],
      name: typeEnum.VL[item]
    })
  }
  const _myChart = getMyLineChart()
  const option = _myChart.getOption()
  option.series = seriesData
  _myChart.setOption(option)
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
</style>
