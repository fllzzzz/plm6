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
    </div>
    <div style="display: flex">
      <div v-loading="loading" :style="{ height: maxHeight / productQuantity + 'px' }" style="flex: 1">
        <div id="structureChart" style="width: 100%; height: 100%"></div>
      </div>
      <common-table v-loading="loading" :height="maxHeight / productQuantity" :data="structureList" style="flex: 1; margin-left: 20px">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column :show-overflow-tooltip="true" label="构件类型" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.typeName }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="产量" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.yield }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="占比" min-width="100px" align="center">
          <template #default="{ row }">
            <span>{{ row.ratio }}%</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
    <template v-if="showEncl">
      <el-divider class="divider" />
      <div style="display: flex; margin-top: 20px">
        <div v-loading="loading" :style="{ height: maxHeight / productQuantity + 'px' }" style="flex: 1">
          <div id="enclosureChart" style="width: 100%; height: 100%"></div>
        </div>
        <common-table v-loading="loading" :height="maxHeight / productQuantity" :data="enclosureList" style="flex: 1; margin-left: 20px">
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column :show-overflow-tooltip="true" label="板型" min-width="100px" align="center">
            <template #default="{ row }">
              <span>{{ row.typeName }}</span>
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" label="产量" min-width="100px" align="center">
            <template #default="{ row }">
              <span>{{ row.yield }}</span>
            </template>
          </el-table-column>
          <el-table-column :show-overflow-tooltip="true" label="占比" min-width="100px" align="center">
            <template #default="{ row }">
              <span>{{ row.ratio }}%</span>
            </template>
          </el-table-column>
        </common-table>
      </div>
    </template>
  </div>
</template>

<script setup>
import { getProductionTypeAnalysis as getApi } from '@/api/operation/production-type-analysis'
import { computed, ref } from 'vue'
import moment from 'moment'

import { mapGetters } from '@/store/lib'
import { componentTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'

function disabledDate(time) {
  return time > new Date()
}

const year = ref(moment().valueOf().toString())

const { productMenu } = mapGetters('productMenu')

const showEncl = computed(() => componentTypeEnum.ENCLOSURE.V & productMenu.value)
const productQuantity = computed(() => showEncl.value ? 2 : 1)

const loading = ref(false)
const structureList = ref([])
const enclosureList = ref([])

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.divider']
})

const pieOption = {
  xAxis: { show: false },
  tooltip: {
    trigger: 'item'
  },
  legend: { cancelDefaultData: true, top: 'center', left: 'right', orient: 'vertical' },
  series: {
    type: 'pie',
    radius: [50, 110],
    center: ['40%', '50%'],
    roseType: 'area',
    itemStyle: {
      borderRadius: 8
    },
    label: {
      show: true,
      formatter: (param) => {
        const { value, ratio } = param.data
        return `${value} | ${ratio}%`
      }
    }
  }
}

const { getMyChart: getStructureChart } = useChart({
  elementId: 'structureChart',
  initOption: {
    custom: {
      title: [{ text: '结构制品' }, { text: '单位：吨', right: 0, textStyle: { fontSize: 13 }}]
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

const { getMyChart: getEnclosureChart } = useChart({
  elementId: 'enclosureChart',
  fetchHook: fetchInfo,
  initOption: {
    custom: {
      title: [{ text: '围护制品' }, { text: '单位：米', right: 0, textStyle: { fontSize: 13 }}]
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

function init() {
  structureList.value = []
  enclosureList.value = []
}

async function fetchInfo(myChart) {
  try {
    init()
    loading.value = true
    await refreshStructureInfo()
    if (showEncl.value) {
      await refreshEnclosureInfo()
    }
  } catch (error) {
    console.log(error, '获取产量分析信息')
  } finally {
    loading.value = false
  }
}

async function refreshStructureInfo(myChart) {
  try {
    const { content } = await getApi({
      dateTime: year.value,
      productType: componentTypeEnum.ARTIFACT.V
    })
    structureList.value = content.map((v) => {
      v.yield = (v.yield / 1000).toFixed(2)
      v.ratio = v.ratio?.toFixed(2)
      return v
    })
    const _myChart = myChart || getStructureChart()
    const option = _myChart.getOption()
    option.series[0].data = structureList.value.map((v) => {
      return {
        value: v.yield,
        name: v.typeName,
        ratio: v.ratio
      }
    })
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取结构制品分析信息')
  }
}

async function refreshEnclosureInfo(myChart) {
  try {
    const { content } = await getApi({
      dateTime: year.value,
      productType: componentTypeEnum.ENCLOSURE.V
    })
    enclosureList.value = content.map((v) => {
      v.yield = (v.yield / 1000).toFixed(2)
      v.ratio = v.ratio?.toFixed(2)
      return v
    })
    const _myChart = myChart || getEnclosureChart()
    const option = _myChart.getOption()
    option.series[0].data = enclosureList.value.map((v) => {
      return {
        value: v.yield,
        name: v.typeName,
        ratio: v.ratio
      }
    })
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取围护制品分析信息')
  }
}
</script>

<style lang="scss" scoped></style>
