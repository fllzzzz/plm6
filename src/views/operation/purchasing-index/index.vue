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

const prePageBtn =
  'image://data:image/png;base64,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'
const nextPageBtn =
  'image://data:image/png;base64,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'

const pieOption = {
  xAxis: { show: false },
  legend: {
    cancelDefaultData: true,
    top: 'center',
    left: 'right',
    orient: 'vertical',
    type: 'scroll',
    pageIcons: {
      vertical: [prePageBtn, nextPageBtn]
    },
    pageIconSize: 12
  },
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
        data: []
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
      v.thickness = v.thickness && Number(v.thickness).toFixed(3)
      return v
    })
    const option = _myChart.getOption()
    option.series[0].data = content.map((v) => {
      const thickness = v.thickness && Number(v.thickness).toFixed(3)
      return {
        value: v.mete,
        name: thickness + '|' + v.spec
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
