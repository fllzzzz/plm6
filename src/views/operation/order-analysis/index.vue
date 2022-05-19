<template>
  <div class="order-analysis-container">
    <div class="app-container main-content">
      <div :style="{ height: mainChartMaxHeight / 2 + 'px' }">
        <div id="annualOrderChart" style="width: 100%; height: 100%"></div>
      </div>
      <common-table v-loading="loading" :data="annualOrderList" class="chart-table chart-table-annual">
        <el-table-column :show-overflow-tooltip="true" label="月份" align="center" width="110">
          <template #default="{ row }">
            <span>{{ row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column v-for="(item, index) in monthArr" :key="index" :show-overflow-tooltip="true" :label="item" align="center">
          <template #default="{ row }">
            <span>{{ row.data[index] || 0 }}</span>
          </template>
        </el-table-column>
      </common-table>
      <el-divider class="divider" />
      <div :style="{ height: mainChartMaxHeight / 2 + 'px' }">
        <div id="orderTypeAnalysisChart" style="width: 100%; height: 100%"></div>
      </div>
      <common-table v-loading="loading" :data="orderTypeList" class="chart-table chart-table-type">
        <el-table-column :show-overflow-tooltip="true" label="月份" align="center" width="110">
          <template #default="{ row }">
            <span>{{ row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column v-for="(item, index) in monthArr" :key="index" :show-overflow-tooltip="true" :label="item" align="center">
          <template #default="{ row }">
            <span>{{ row.data[index] || 0 }}</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
    <div class="project-chart">
      <div class="chart-head">
        <el-date-picker
          v-model="year"
          type="year"
          size="small"
          style="width: 300px"
          placeholder="选择年"
          value-format="x"
          :clearable="false"
          :disabled-date="disabledDate"
        />
      </div>
      <div class="chart-container" :style="{ height: projectChartMaxHeight + 'px' }">
        <div id="orderQuantityChart" style="width: 100%; height: 100%"></div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { getProjectInfo } from '@/api/common'
import { getOrderAnalysis } from '@/api/operation/order-analysis'
import moment from 'moment'
import { ref, reactive, computed, watch } from 'vue'

import { businessTypeEnum } from '@enum-ms/contract'
import { obj2arr } from '@/utils/convert/type'
import { deepClone } from '@/utils/data-type'

import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'

function disabledDate(time) {
  return time > new Date()
}

const year = ref(moment().valueOf().toString())

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const loading = ref(false)
const annualOrderList = reactive([
  { name: '订单数', data: [] },
  { name: '合同额（万元）', data: [] }
])

const defaultOrderTypeObj = {}
for (const key in businessTypeEnum.ENUM) {
  const item = businessTypeEnum.ENUM[key]
  if (item) {
    defaultOrderTypeObj[item.V] = {
      name: item.L,
      data: new Array(12).fill(0)
    }
  }
}

const orderTypeObj = ref(Object.assign({}, deepClone(defaultOrderTypeObj)))

const orderTypeList = computed(() => {
  return obj2arr(orderTypeObj.value)
})

const { maxHeight: mainChartMaxHeight } = useMaxHeight({
  mainBox: '.main-content',
  extraBox: ['.chart-table-annual', '.chart-table-type', '.divider'],
  wrapperBox: '.main-content',
  minHeight: 400
})

const { maxHeight: projectChartMaxHeight } = useMaxHeight({
  extraBox: '.chart-head',
  wrapperBox: '.chart-container',
  // 右侧最小宽度 - 顶部时间选择框
  minHeight: 566,
  extraHeight: 40
})

const commonOption = {
  legend: { right: 0 },
  xAxis: {
    show: false,
    data: monthArr.value
  }
}

const { getMyChart: getMyChart1 } = useChart({
  elementId: 'annualOrderChart',
  initOption: {
    ...commonOption,
    custom: {
      yAxis: annualOrderList?.reduce((arr, data) => {
        arr.push({
          type: 'value'
          // name: data.name
        })
        return arr
      }, [])
    },
    title: {
      text: '年度订单量'
    },
    series: annualOrderList?.reduce((arr, data, index) => {
      arr.push({
        type: 'bar',
        yAxisIndex: index,
        ...data
      })
      return arr
    }, [])
  }
})
const { getMyChart: getMyChart2 } = useChart({
  elementId: 'orderTypeAnalysisChart',
  initOption: {
    ...commonOption,
    title: {
      text: '订单类型分析'
    },
    series: setListChartSeries(orderTypeList.value)
  }
})

const { getMyChart: getMyChart3 } = useChart({
  elementId: 'orderQuantityChart',
  fetchHook: fetchInfo,
  initOption: {
    grid: {
      left: '0%',
      right: '0%',
      bottom: 0,
      top: 0,
      containLabel: true
    },
    tooltip: {
      trigger: 'item' // 悬浮提示框不显示
    },
    toolbox: {
      show: false
    },
    xAxis: {
      show: false,
      type: 'value',
      boundaryGap: [0, 0],
      position: 'top'
    },
    yAxis: {
      type: 'category',
      axisLine: { show: false },
      inverse: true,
      axisTick: [
        {
          // 坐标轴小标记
          show: false
        }
      ],
      axisLabel: {
        fontSize: '14'
      },
      data: []
    },
    legend: { show: false },
    series: [
      {
        name: '',
        type: 'bar',
        tooltip: { show: false },
        barMinHeight: 30, // 最小柱高
        barWidth: 40, // 柱宽度
        barMaxWidth: 100, // 最大柱宽度
        data: [],
        label: {
          show: true, // 显示文本
          position: 'inside', // 数据值位置
          fontSize: '14'
        },
        itemStyle: {
          // 柱状图颜色
          color: '#1890ff'
        }
      }
    ]
  }
})

watch(
  () => year.value,
  (val, oldVal) => {
    if (val && oldVal) {
      fetchInfo()
    }
  }
)

function setInitArray(list) {
  for (let i = 0; i < list.length; i++) {
    list[i].data = new Array(12).fill(0)
  }
}

function init() {
  setInitArray(annualOrderList)
  orderTypeObj.value = Object.assign({}, deepClone(defaultOrderTypeObj))
}

async function fetchInfo() {
  try {
    init()
    loading.value = true
    const { orders } = await getOrderAnalysis({ dateTime: year.value })
    for (let i = 0; i < orders.length; i++) {
      const item = orders[i]
      annualOrderList[0].data[item.month - 1] += item.count
      annualOrderList[1].data[item.month - 1] += item.contractAmount
      orderTypeObj.value[item.businessType].data[item.month - 1] += item.count
    }
    for (let i = 0; i < annualOrderList[1].data.length; i++) {
      if (!annualOrderList[1].data[i]) continue
      annualOrderList[1].data[i] = (annualOrderList[1].data[i] / 10000).toFixed(2)
    }
    refreshProjectSummary()
    refreshAnnualInfo()
    refreshOrderTypeInfo()
  } catch (error) {
    console.log(error, '获取信息失败')
  } finally {
    loading.value = false
  }
}

async function refreshProjectSummary(myChart) {
  try {
    const _myChart = myChart || getMyChart3()
    const option = _myChart.getOption()
    const { provinceList } = await getProjectInfo({ year: moment(Number(year.value)).year() })
    option.yAxis[0].max = parseInt(projectChartMaxHeight.value / 50)
    option.yAxis[0].data = provinceList.map((v) => v.provinceName)
    option.series[0].data = provinceList.map((v) => v.quantity)
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取项目汇总列表失败')
  }
}

function setListChartSeries(list) {
  return list?.reduce((arr, data) => {
    arr.push({
      type: 'bar',
      ...data
    })
    return arr
  }, [])
}

async function refreshAnnualInfo(myChart) {
  try {
    const _myChart = myChart || getMyChart1()
    _myChart.setOption({
      series: setListChartSeries(annualOrderList)
    })
  } catch (error) {
    console.log(error, '刷新年度信息失败')
  }
}

async function refreshOrderTypeInfo(myChart) {
  try {
    const _myChart = myChart || getMyChart2()
    _myChart.setOption({
      series: setListChartSeries(orderTypeList.value)
    })
  } catch (error) {
    console.log(error, '刷新订单类型失败')
  }
}
</script>

<style lang="scss" scoped>
.order-analysis-container {
  display: flex;

  .main-content {
    flex: 1;

    .chart-table {
      width: 100%;
      margin-top: 10px;
    }
  }

  .project-chart {
    width: 340px;
    padding: 20px 0 20px 20px;
    overflow: hidden;
    border-left: 1px solid #ededed;
    .chart-head {
      padding-right: 20px;
      padding-bottom: 0px;
    }
    .chart-container {
      width: 320px;
      padding-right: 20px;
      overflow-y: auto;
    }
  }
}

::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>
