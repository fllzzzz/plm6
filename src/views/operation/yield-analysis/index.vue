<template>
  <div class="app-container">
    <div class="head-container">
      <el-date-picker
        v-model="year"
        type="year"
        size="small"
        style="width: 300px"
        placeholder="选择年"
        class="filter-item"
        value-format="x"
        :clearable="false"
        :disabled-date="disabledDate"
        @change="fetchInfo"
      />
      <factory-select v-model="factoryId" class="filter-item" showOptionAll style="width: 300px" @change="fetchInfo" />
    </div>
    <div style="display: flex">
      <div v-loading="loading" :style="{ height: maxHeight / 2 + 'px' }" style="flex: 1">
        <div id="yieldStructureChart" style="width: 100%; height: 100%"></div>
      </div>
      <common-table
        v-loading="loading"
        :maxHeight="maxHeight / 2"
        :data="structureList"
        show-summary
        :summary-method="getSummaries"
        style="width: 500px; margin-left: 20px"
      >
        <el-table-column :show-overflow-tooltip="true" label="月份" align="center">
          <template #default="{ row }">
            <span>{{ row.month }}</span>
          </template>
        </el-table-column>
        <template v-for="(item, key) in structureData" :key="key">
          <el-table-column :prop="key" :show-overflow-tooltip="true" :label="key" align="center">
            <template #default="{ $index }">
              <span>{{ item[$index] || 0 }}</span>
            </template>
          </el-table-column>
        </template>
      </common-table>
    </div>
    <el-divider class="divider" />
    <div style="display: flex; margin-top: 20px">
      <div v-loading="loading" :style="{ height: maxHeight / 2 + 'px' }" style="flex: 1">
        <div id="yieldEnclosureChart" style="width: 100%; height: 100%"></div>
      </div>
      <common-table
        v-loading="loading"
        :maxHeight="maxHeight / 2"
        :data="enclosureList"
        show-summary
        :summary-method="getSummaries"
        style="width: 500px; margin-left: 20px"
      >
        <el-table-column :show-overflow-tooltip="true" label="月份" align="center">
          <template #default="{ row }">
            <span>{{ row.month }}</span>
          </template>
        </el-table-column>
        <template v-for="(item, key) in enclosureData" :key="key">
          <el-table-column :prop="key" :show-overflow-tooltip="true" :label="key" align="center">
            <template #default="{ $index }">
              <span>{{ item[$index] || 0 }}</span>
            </template>
          </el-table-column>
        </template>
      </common-table>
    </div>
  </div>
</template>

<script setup>
import { getYieldAnalysis } from '@/api/operation/yield-analysis'
import { ref } from 'vue'
import moment from 'moment'

import { componentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'
import factorySelect from '@comp-base/factory-select'

function disabledDate(time) {
  return time > new Date()
}

const yearSize = 3
const year = ref(moment().valueOf().toString())
const factoryId = ref()

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

const loading = ref(false)
const structureList = ref([])
const enclosureList = ref([])
const structureData = ref({})
const enclosureData = ref({})

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.divider']
})

// 合计
function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
    } else {
      const values = data.map((item) => {
        return Number(item.yearData[column.property]) || 0
      })
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
    }
  })
  return sums
}

const commonOption = {
  // legend: { right: 0 },
  xAxis: { data: monthArr.value },
  series: setListChartSeries()
}

const { getMyChart: getStructureChart } = useChart({
  elementId: 'yieldStructureChart',
  initOption: {
    ...commonOption,
    custom: {
      title: [{ text: '产量分析（结构制品）' }, { text: '单位：吨', right: 0, textStyle: { fontSize: 13 }}]
    }
  }
})

const { getMyChart: getEnclosureChart } = useChart({
  elementId: 'yieldEnclosureChart',
  fetchHook: fetchInfo,
  initOption: {
    ...commonOption,
    custom: {
      title: [{ text: '产量分析（围护制品）' }, { text: '单位：米', right: 0, textStyle: { fontSize: 13 }}]
    }
  }
})

function setListChartSeries(data) {
  if (!data || !Object.keys(data).length) return []
  const arr = []
  for (const item in data) {
    arr.push({
      type: 'bar',
      name: item,
      data: data[item]
    })
  }
  return arr
}

function init() {
  structureData.value = {}
  structureList.value = []
  enclosureData.value = {}
  enclosureList.value = []
}

async function fetchInfo(myChart) {
  try {
    init()
    loading.value = true
    await refreshStructureInfo()
    await refreshEnclosureInfo()
  } catch (error) {
    console.log(error, '获取产量分析信息')
  } finally {
    loading.value = false
  }
}

async function refreshStructureInfo(myChart) {
  try {
    const _myChart = myChart || getStructureChart()
    const { content } = await getYieldAnalysis({
      dateTime: year.value,
      factoryId: factoryId.value,
      productType: componentTypeEnum.ARTIFACT.V,
      yearSize
    })
    const { resData, resList } = formatData(content)
    structureData.value = resData
    structureList.value = resList
    _myChart.setOption({
      series: setListChartSeries(structureData.value)
    })
  } catch (error) {
    console.log(error, '获取结构产量分析信息')
  }
}

async function refreshEnclosureInfo(myChart) {
  try {
    const _myChart = myChart || getEnclosureChart()
    const { content } = await getYieldAnalysis({
      dateTime: year.value,
      factoryId: factoryId.value,
      productType: componentTypeEnum.ENCLOSURE.V,
      yearSize
    })
    const { resData, resList } = formatData(content)
    enclosureData.value = resData
    enclosureList.value = resList
    _myChart.setOption({
      series: setListChartSeries(enclosureData.value)
    })
  } catch (error) {
    console.log(error, '获取围护产量分析信息')
  }
}

function formatData(list) {
  const resData = {}
  let resList = []
  for (let i = 0; i < list.length; i++) {
    const item = list[i]
    resData[item.year] = new Array(12).fill(0)
    for (let x = 0; x < item.productionDetailsList.length; x++) {
      const cItem = item.productionDetailsList[x]
      resData[item.year][moment(cItem.month).month()] = (cItem.production / 1000).toFixed(2)
    }
  }
  resList = monthArr.value.reduce((arr, data, index) => {
    const yearData = {}
    for (const item in resData) {
      yearData[item] = resData[item][index] || 0
    }
    arr.push({
      month: data,
      yearData
    })
    return arr
  }, [])
  return { resData, resList }
}
</script>

<style lang="scss" scoped></style>
