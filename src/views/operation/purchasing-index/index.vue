<template>
  <div class="app-container">
    <div class="head-container">
      <el-date-picker
        v-model="steelTrendQuery.year"
        type="year"
        size="small"
        style="width: 300px"
        placeholder="选择年"
        value-format="x"
        :clearable="false"
        :disabled-date="disabledDate"
        @change="refreshSteelPurchasingTrend"
        class="filter-item"
      />
      <steel-query @toQuery="refreshSteelPurchasingTrend"></steel-query>
      <div style="float: right; font-size: 16px; color: #303133">单位：元/吨</div>
    </div>
    <div style="display: flex">
      <div :style="{ height: chartHeight + 'px' }" style="flex: 1; margin-right: 20px">
        <div id="steelPurchasingTrendChart" style="width: 100%; height: 100%"></div>
      </div>
      <div :style="{ height: chartHeight + 'px' }" style="flex: 1">
        <div id="steelPurchasingTrend3YearChart" style="width: 100%; height: 100%"></div>
      </div>
    </div>
    <el-divider class="divider" />
    <div class="head-container">
      <el-date-picker
        v-model="sectionTrendQuery.year"
        type="year"
        size="small"
        style="width: 300px"
        placeholder="选择年"
        value-format="x"
        :clearable="false"
        :disabled-date="disabledDate"
        @change="refreshSectionPurchasingTrend"
        class="filter-item"
      />
      <section-query @toQuery="refreshSectionPurchasingTrend"></section-query>
      <div style="float: right; font-size: 16px; color: #303133">单位：元/吨</div>
    </div>
    <div style="display: flex">
      <div :style="{ height: chartHeight + 'px' }" style="flex: 1; margin-right: 20px">
        <div id="sectionPurchasingTrendChart" style="width: 100%; height: 100%"></div>
      </div>
      <div :style="{ height: chartHeight + 'px' }" style="flex: 1">
        <div id="sectionPurchasingTrend3YearChart" style="width: 100%; height: 100%"></div>
      </div>
    </div>
    <el-divider class="divider" />
    <div class="head-container">
      <el-date-picker
        v-model="steelProportionQuery.year"
        type="year"
        size="small"
        style="width: 300px"
        placeholder="选择年"
        value-format="x"
        :clearable="false"
        :disabled-date="disabledDate"
        @change="refreshSteelPurchasingProportion"
      />
      <div style="float: right; font-size: 16px; color: #303133">单位：吨</div>
    </div>
    <div style="display: flex">
      <div :style="{ height: chartHeight + 'px' }" style="flex: 1; margin-right: 20px">
        <div id="steelPurchasingProportionChart" style="width: 100%; height: 100%"></div>
      </div>
      <common-table v-loading="loading" :height="chartHeight" :data="steelProportionList" style="flex: 1">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column :show-overflow-tooltip="true" label="板厚" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.thickness }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="材质" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.spec }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="采购量" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.mete }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="占比" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.ratio }}%</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
    <el-divider class="divider" />
    <div class="head-container">
      <el-date-picker
        v-model="sectionProportionQuery.year"
        type="year"
        size="small"
        style="width: 300px"
        placeholder="选择年"
        value-format="x"
        :clearable="false"
        :disabled-date="disabledDate"
        @change="refreshSectionPurchasingProportion"
      />
      <div style="float: right; font-size: 16px; color: #303133">单位：吨</div>
    </div>
    <div style="display: flex">
      <div :style="{ height: chartHeight + 'px' }" style="flex: 1; margin-right: 20px">
        <div id="sectionPurchasingProportionChart" style="width: 100%; height: 100%"></div>
      </div>
      <common-table v-loading="loading" :height="chartHeight" :data="sectionProportionList" style="flex: 1">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column :show-overflow-tooltip="true" label="型材" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.classifyName }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="材质" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.spec }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="采购量" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.mete }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="占比" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.ratio }}%</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
  </div>
</template>

<script setup>
import { getPurchaseRatio, getPurchaseTrend } from '@/api/operation/purchasing-index'
import { provide, ref } from 'vue'
import moment from 'moment'

import { matClsEnum } from '@enum-ms/classification'

import useChart from '@compos/use-chart'
import steelQuery from './module/steel-query.vue'
import sectionQuery from './module/section-query.vue'

const chartHeight = 300
const yearSize = 3

const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}

function disabledDate(time) {
  return time > new Date()
}
const initYear = moment().valueOf().toString()

const steelTrendQuery = ref({
  year: initYear
})
const sectionTrendQuery = ref({
  year: initYear
})
const steelProportionQuery = ref({
  year: initYear
})
const sectionProportionQuery = ref({
  year: initYear
})
const steelProportionList = ref([])
const sectionProportionList = ref([])
const loading = ref(false)

provide('steel-query', steelTrendQuery.value)
provide('section-query', sectionTrendQuery.value)

const { getMyChart: getSteelPurchasingTrendChart } = useChart({
  elementId: 'steelPurchasingTrendChart',
  initOption: {
    title: {
      text: '钢板采购价走势'
    },
    xAxis: { data: monthArr.value },
    series: [{ name: '', type: 'line', data: [] }]
  }
})

const { getMyChart: getSteelPurchasingTrend3YearChart } = useChart({
  elementId: 'steelPurchasingTrend3YearChart',
  fetchHook: refreshSteelPurchasingTrend,
  initOption: {
    title: {
      text: '近三年采购价走势'
    },
    xAxis: { data: monthArr.value },
    series: [
      { name: '', type: 'line', data: [] },
      { name: '', type: 'line', data: [] },
      { name: '', type: 'line', data: [] }
    ]
  }
})

const { getMyChart: getSectionPurchasingTrendChart } = useChart({
  elementId: 'sectionPurchasingTrendChart',
  initOption: {
    title: {
      text: '型材采购价走势'
    },
    xAxis: { data: monthArr.value },
    series: [{ name: '', type: 'line', data: [] }]
  }
})

const { getMyChart: getSectionPurchasingTrend3YearChart } = useChart({
  elementId: 'sectionPurchasingTrend3YearChart',
  fetchHook: refreshSectionPurchasingTrend,
  initOption: {
    title: {
      text: '近三年采购价走势'
    },
    xAxis: { data: monthArr.value },
    series: [
      { name: '', type: 'line', data: [] },
      { name: '', type: 'line', data: [] },
      { name: '', type: 'line', data: [] }
    ]
  }
})

const pieOption = {
  xAxis: { show: false },
  legend: { cancelDefaultData: true, top: 'center', left: 'right', orient: 'vertical' },
  tooltip: {
    trigger: 'item'
  },
  series: {
    type: 'pie',
    radius: [30, 110],
    center: ['40%', '50%'],
    roseType: 'area',
    itemStyle: {
      borderRadius: 8
    },
    label: {
      show: true
    }
  }
}

const { getMyChart: getSteelPurchasingProportionChart } = useChart({
  elementId: 'steelPurchasingProportionChart',
  fetchHook: refreshSteelPurchasingProportion,
  initOption: {
    title: {
      text: '钢板采购占比'
    },
    ...pieOption,
    series: [
      {
        name: '',
        ...pieOption.series,
        data: []
      }
    ]
  }
})

const { getMyChart: getSectionPurchasingProportionChart } = useChart({
  elementId: 'sectionPurchasingProportionChart',
  fetchHook: refreshSectionPurchasingProportion,
  initOption: {
    title: {
      text: '型材采购占比'
    },
    ...pieOption,
    series: [
      {
        name: '',
        ...pieOption.series,
        data: [
          { value: 40, name: 'rose 1' },
          { value: 38, name: 'rose 2' },
          { value: 32, name: 'rose 3' },
          { value: 30, name: 'rose 4' },
          { value: 28, name: 'rose 5' },
          { value: 26, name: 'rose 6' },
          { value: 22, name: 'rose 7' },
          { value: 22, name: 'rose 27' },
          { value: 22, name: 'rose 37' },
          { value: 22, name: 'rose 47' },
          { value: 22, name: 'rose 57' },
          { value: 22, name: 'rose 67' },
          { value: 22, name: 'rose 77' },
          { value: 22, name: 'rose 7' },
          { value: 18, name: 'rose 8' }
        ]
      }
    ]
  }
})

async function refreshSteelPurchasingTrend() {
  try {
    const { content } = await getPurchaseTrend({
      dateTime: steelTrendQuery.value.year,
      basicClass: matClsEnum.STEEL_PLATE.V,
      range: yearSize,
      ...steelTrendQuery.value
    })
    const _myChart = getSteelPurchasingTrendChart()
    const option = _myChart.getOption()
    const xData = new Array(12).fill(0)
    for (let i = 0; i < content[0]?.summaryList?.length; i++) {
      const item = content[0].summaryList[i]
      xData[item.date - 1] = (item.mete && Number((item.amount / (item.mete / 1000000)).toFixed(0))) || 0
    }
    option.series[0].data = xData
    _myChart.setOption(option)

    const _myChart3Year = getSteelPurchasingTrend3YearChart()
    const option3Year = _myChart3Year.getOption()
    for (let x = 0; x < content.length; x++) {
      const pItem = content[x]
      const xData3Year = new Array(12).fill(0)
      for (let i = 0; i < content[x]?.summaryList?.length; i++) {
        const item = content[x].summaryList[i]
        xData3Year[item.date - 1] = (item.mete && Number((item.amount / (item.mete / 1000000)).toFixed(0))) || 0
      }
      option3Year.series[x].name = pItem.year
      option3Year.series[x].data = xData3Year
    }
    _myChart3Year.setOption(option3Year)
  } catch (error) {
    console.log(error, '获取钢板采购趋势信息失败')
  }
}

async function refreshSectionPurchasingTrend() {
  try {
    const { content } = await getPurchaseTrend({
      dateTime: sectionTrendQuery.value.year,
      basicClass: matClsEnum.SECTION_STEEL.V,
      range: yearSize,
      ...sectionTrendQuery.value
    })
    const _myChart = getSectionPurchasingTrendChart()
    const option = _myChart.getOption()
    const xData = new Array(12).fill(0)
    for (let i = 0; i < content[0]?.summaryList?.length; i++) {
      const item = content[0].summaryList[i]
      xData[item.date - 1] = (item.mete && Number((item.amount / (item.mete / 1000000)).toFixed(0))) || 0
    }
    option.series[0].data = xData
    _myChart.setOption(option)

    const _myChart3Year = getSectionPurchasingTrend3YearChart()
    const option3Year = _myChart3Year.getOption()
    for (let x = 0; x < content.length; x++) {
      const pItem = content[x]
      const xData3Year = new Array(12).fill(0)
      for (let i = 0; i < content[x]?.summaryList?.length; i++) {
        const item = content[x].summaryList[i]
        xData3Year[item.date - 1] = (item.mete && Number((item.amount / (item.mete / 1000000)).toFixed(0))) || 0
      }
      option3Year.series[x].name = pItem.year
      option3Year.series[x].data = xData3Year
    }
    _myChart3Year.setOption(option3Year)
  } catch (error) {
    console.log(error, '获取型材采购趋势信息失败')
  }
}

async function refreshSteelPurchasingProportion() {
  try {
    steelProportionList.value = []
    const _myChart = getSteelPurchasingProportionChart()
    const { content } = await getPurchaseRatio({
      dateTime: steelProportionQuery.value.year,
      basicClass: matClsEnum.STEEL_PLATE.V
    })
    steelProportionList.value = content.map((v) => {
      v.mete = Number((v.mete / 1000000).toFixed(2))
      v.ratio = v.ratio?.toFixed(2)
      return v
    })
    const option = _myChart.getOption()
    option.series[0].data = content.map((v) => {
      return {
        value: v.mete,
        name: v.thickness + '|' + v.spec
      }
    })
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取钢板采购占比信息失败')
  }
}

async function refreshSectionPurchasingProportion() {
  try {
    sectionProportionList.value = []
    const _myChart = getSectionPurchasingProportionChart()
    const { content } = await getPurchaseRatio({
      dateTime: sectionProportionQuery.value.year,
      basicClass: matClsEnum.SECTION_STEEL.V
    })
    sectionProportionList.value = content.map((v) => {
      v.mete = Number((v.mete / 1000000).toFixed(2))
      v.ratio = v.ratio?.toFixed(2)
      return v
    })
    const option = _myChart.getOption()
    option.series[0].data = content.map((v) => {
      return {
        value: v.mete,
        name: v.classifyName + '|' + v.spec
      }
    })
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取型材采购占比信息失败')
  }
}
</script>

<style lang="scss" scoped></style>
