<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader class="head-container" />
    <!--表格渲染-->
    <div class="type-analysis" style="display: flex">
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="311"
        :data-format="dataFormat"
        highlight-current-row
        show-summary
        :summary-method="getSummaries"
        style="width: 404px; margin-right: 20px"
        @current-change="showDetail"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="类型" align="center" />
        <el-table-column
          v-if="columns.visible('completeLength')"
          key="completeLength"
          prop="completeLength"
          :show-overflow-tooltip="true"
          label="产量(米)"
          align="center"
        />
        <el-table-column
          v-if="columns.visible('proportion')"
          key="proportion"
          prop="proportion"
          :show-overflow-tooltip="true"
          label="占比"
          align="center"
        />
      </common-table>
      <div v-loading="crud.loading" :style="{ height: '311px' }" style="flex: 1">
        <div id="typeAnalysisChart" style="width: 100%; height: 100%"></div>
      </div>
    </div>
    <!-- 类型规格分析 -->
    <div :style="{ opacity: itemInfo.category ? 100 : 0 }">
      <el-divider />
      <div style="display: flex">
        <common-table
          ref="detailTableRef"
          v-loading="detailTableLoading"
          :data="detailList"
          :max-height="maxHeight"
          :data-format="dataFormat"
          show-summary
          :summary-method="getSummaries"
          style="width: 404px; margin-right: 20px"
        >
          <el-table-column label="序号" type="index" align="center" width="60" />
          <el-table-column show-overflow-tooltip prop="plate" key="plate" label="版型" align="center" />
          <el-table-column show-overflow-tooltip prop="completeLength" key="completeLength" label="产量(米)" align="center" />
          <el-table-column show-overflow-tooltip prop="proportion" key="proportion" label="占比" align="center" />
        </common-table>
        <div v-loading="detailTableLoading" :style="{ height: maxHeight + 'px' }" style="flex: 1">
          <div id="typeSpecificationChart" style="width: 100%; height: 100%"></div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/enclosure/production-report/type-analysis'
import { ref } from 'vue'

import { enclosureTypeAnalysisPM as permission } from '@/page-permission/enclosure'
import { tableSummary } from '@/utils/el-extra'
import { mesEnclosureTypeEnum } from '@enum-ms/mes'

import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import useChart from '@compos/use-chart'
import useMaxHeight from '@compos/use-max-height'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const detailTableRef = ref()
const detailVisible = ref(false)
const itemInfo = ref({})
const enclosureTypeArr = ref(Object.values(mesEnclosureTypeEnum.V))
const detailTableLoading = ref(false)
const detailList = ref([])

const dataFormat = ref([['proportion', ['suffix', '%']]])

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.head-container', '.type-analysis', '.el-divider'],
    minHeight: 200,
    paginate: false
  },
  detailTableRef
)

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['completeLength', 2]]
  })
}

const { CRUD, crud, columns } = useCRUD(
  {
    title: '类型分析',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { getMyChart } = useChart({
  elementId: 'typeAnalysisChart',
  initOption: {
    legend: { show: false },
    title: {
      text: '单位：m'
    },
    yAxis: {
      type: 'category',
      axisTick: {
        show: false
      },
      data: enclosureTypeArr.value.map((v) => v.L)
    },
    xAxis: {
      type: 'value'
    },
    series: [{ name: '产量', type: 'bar', data: [] }]
  }
})

const { getMyChart: getDetailEChart } = useChart({
  elementId: 'typeSpecificationChart',
  initOption: {
    legend: { show: false },
    title: {
      text: '单位：m'
    },
    yAxis: {
      type: 'category',
      axisTick: {
        show: false
      },
      data: []
    },
    xAxis: { type: 'value' },
    series: [{ name: '产量', type: 'bar', data: [] }]
  }
})

// 获取详情列表
async function fetchDetailList() {
  try {
    detailTableLoading.value = true
    const data =
      (await crudApi.detail({
        category: itemInfo.value.category,
        ...crud.query
      })) || []

    const _series = []
    const _yAxis = []
    detailList.value = data.map((row) => {
      _series.push(row.completeLength)
      _yAxis.push(row.plate)
      return row
    })

    const _myChart = getDetailEChart()
    const option = _myChart.getOption()
    option.series[0].data = _series
    option.yAxis[0].data = _yAxis
    option.title[0].text = `${itemInfo.value.name} 单位：m`
    _myChart.setOption(option)
  } catch (error) {
    console.log('获取类型规格分析', error)
  } finally {
    detailTableLoading.value = false
  }
}

function showDetail(row = {}) {
  detailVisible.value = true
  itemInfo.value = {
    ...row
  }
  fetchDetailList()
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  const enclosureTypeKV = {}
  data.content = data.map((row) => {
    enclosureTypeKV[row.category] = row
    return row
  })
  const _myChart = getMyChart()
  const option = _myChart.getOption()
  option.series[0].data = enclosureTypeArr.value.map((row) => enclosureTypeKV[row.V]?.completeLength || 0)
  _myChart.setOption(option)
}
</script>
