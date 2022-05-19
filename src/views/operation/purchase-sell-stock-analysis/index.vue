<template>
  <div class="app-container">
    <div class="head-container">
      <el-date-picker
        v-model="year"
        type="year"
        size="small"
        style="width: 300px"
        placeholder="选择年"
        :clearable="false"
        value-format="x"
        :disabled-date="disabledDate"
        class="filter-item"
        @change="fetchAll"
      />
      <common-select v-model="currentType" :options="typeEnum.ENUM" showOptionAll type="enum" class="filter-item" @change="fetchByType" />
    </div>
    <div :style="{ height: mainChartMaxHeight / 2 + 'px' }" style="flex: 1">
      <div id="purchaseUseChart" style="width: 100%; height: 100%"></div>
    </div>
    <el-divider class="divider" />
    <div style="display: flex">
      <div :style="{ height: mainChartMaxHeight / 2 + 'px' }" style="flex: 1; margin-right: 20px">
        <div id="stockChart" style="width: 100%; height: 100%"></div>
      </div>
      <div :style="{ height: mainChartMaxHeight / 2 + 'px' }" style="flex: 1">
        <div id="steelStockChart" style="width: 100%; height: 100%"></div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { getInventoryAnalysis, getInventoryStock, getInventorySteelStock } from '@/api/operation/purchase-sell-stock-analysis'
import { ref } from 'vue'
import moment from 'moment'
import { constantize } from '@enum/base'

import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'

function disabledDate(time) {
  return time > new Date()
}

const typeEnum = {
  STEEL: { L: '钢材', K: 'STEEL', V: 1 },
  MATERIAL: { L: '辅材', K: 'MATERIAL', V: 2 }
}
constantize(typeEnum)
const taxEnum = {
  YES: { L: '含税', K: 'YES', V: 1 },
  NO: { L: '不含税', K: 'NO', V: 2 }
}
constantize(taxEnum)
const year = ref(moment().valueOf().toString())
const currentType = ref()
const currentTax = taxEnum.YES.V

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const { maxHeight: mainChartMaxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.divider'],
  minHeight: 400
})

const { getMyChart: getPurchaseUseChart } = useChart({
  elementId: 'purchaseUseChart',
  fetchHook: fetchPurchaseUseInfo,
  initOption: {
    custom: {
      title: [{ text: '采购和使用分析' }, { text: '单位：万元', right: 0, textStyle: { fontSize: 13 }}]
    },
    xAxis: { data: monthArr.value },
    series: [
      { name: '月度采购数据', type: 'line', data: [] },
      { name: '月度使用数据', type: 'line', data: [] }
    ]
  }
})

const { getMyChart: getStockChart } = useChart({
  elementId: 'stockChart',
  fetchHook: fetchStockInfo,
  initOption: {
    custom: {
      title: [{ text: '库存分析' }, { text: '单位：万元', right: 0, textStyle: { fontSize: 13 }}]
    },
    xAxis: { data: monthArr.value },
    series: [{ name: '', type: 'line', data: [] }]
  }
})

const { getMyChart: getSteelStockChart } = useChart({
  elementId: 'steelStockChart',
  fetchHook: fetchSteelStockInfo,
  initOption: {
    custom: {
      title: [{ text: '钢材库存' }, { text: '单位：吨', right: 0, textStyle: { fontSize: 13 }}]
    },
    xAxis: { data: monthArr.value },
    series: [{ name: '', type: 'line', data: [] }]
  }
})

async function fetchPurchaseUseInfo(myChart) {
  try {
    const _myChart = myChart || getPurchaseUseChart()
    const { content } = await getInventoryAnalysis({
      dateTime: year.value,
      tax: currentTax,
      type: currentType.value
    })
    const intoAmount = content.map((v) => (v.intoAmount && Number((v.intoAmount / 1000).toFixed(2))) || 0)
    const outAmount = content.map((v) => (v.outAmount && Number((v.outAmount / 1000).toFixed(2))) || 0)
    const option = _myChart.getOption()
    option.series[0].data = intoAmount
    option.series[1].data = outAmount
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取采购和使用分析信息失败')
  }
}

async function fetchStockInfo(myChart) {
  try {
    const _myChart = myChart || getStockChart()
    const { content } = await getInventoryStock({
      dateTime: year.value,
      tax: currentTax,
      type: currentType.value
    })
    const amount = content.map((v) => (v.amount && Number((v.amount / 1000).toFixed(2))) || 0)
    const option = _myChart.getOption()
    option.series[0].data = amount
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取库存分析信息失败')
  }
}

async function fetchSteelStockInfo(myChart) {
  try {
    const _myChart = myChart || getSteelStockChart()
    const { content } = await getInventorySteelStock({
      dateTime: year.value,
      tax: currentTax
    })
    const weight = content.map((v) => (v.weight && Number((v.weight / 1000).toFixed(0))) || 0)
    const option = _myChart.getOption()
    option.series[0].data = weight
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取钢材库存信息失败')
  }
}

function fetchAll() {
  fetchPurchaseUseInfo()
  fetchStockInfo()
  fetchSteelStockInfo()
}

function fetchByType() {
  fetchPurchaseUseInfo()
  fetchStockInfo()
}
</script>

<style lang="scss" scoped></style>
