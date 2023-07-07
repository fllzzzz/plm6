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
        @change="fetchInfo"
      />
      <branch-company-select v-model="branchCompanyId" clearable class="filter-item" @change="fetchInfo" style="width: 350px" />
    </div>
    <div style="display: flex">
      <div v-loading="loading" :style="{ height: maxHeight + 'px' }" style="flex: 1">
        <div id="incomeExpenditureChart" style="width: 100%; height: 100%"></div>
      </div>
      <common-table
        v-loading="loading"
        :data="list"
        :height="maxHeight"
        :data-format="dataFormat"
        show-summary
        :summary-method="getSummaries"
        style="width: 500px; margin-left: 20px"
      >
        <el-table-column :show-overflow-tooltip="true" label="月份" align="center">
          <template #default="{ row }">
            <span>{{ row.month }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="income" :show-overflow-tooltip="true" label="收入" align="center">
          <template #default="{ row }">
            <span>{{ row.income }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="expend" :show-overflow-tooltip="true" label="支出" align="center">
          <template #default="{ row }">
            <span>{{ row.expend }}</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
  </div>
</template>

<script setup>
import { getIncomeAnalysis } from '@/api/operation/income-expenditure-analysis'
import { ref, computed } from 'vue'
import moment from 'moment'

import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'

import branchCompanySelect from '@comp-base/branch-company-select.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

function disabledDate(time) {
  return time > new Date()
}

const year = ref(moment().valueOf().toString())
const branchCompanyId = ref()
const loading = ref(false)
const monthArr = ref([])
for (let i = 1; i <= 12; i++) {
  monthArr.value.push(i + '月')
}
const list = ref([])

const { maxHeight } = useMaxHeight({
  extraHeight: 0
})

const dataFormat = computed(() => {
  return [
    ['income', ['to-thousand', decimalPrecision.value.operation]],
    ['expend', ['to-thousand', decimalPrecision.value.operation]]
  ]
})

// 合计
function getSummaries(param) {
  return tableSummary(param, { props: [['income', decimalPrecision.value.operation], ['expend', decimalPrecision.value.operation]], toThousandFields: ['income', 'expend'] })
}

const { getMyChart } = useChart({
  elementId: 'incomeExpenditureChart',
  fetchHook: fetchInfo,
  initOption: {
    legend: { right: 0 },

    title: {
      text: '收入与支出'
    },
    yAxis: {
      type: 'category',
      axisTick: {
        show: false
      },
      data: monthArr.value
    },
    xAxis: { type: 'value' },
    series: [
      { name: '收入', type: 'bar', data: [] },
      { name: '支出', type: 'bar', data: [] }
    ]
  }
})

async function fetchInfo() {
  try {
    loading.value = true
    const _myChart = getMyChart()
    const { content } = await getIncomeAnalysis({
      dateTime: year.value,
      branchCompanyId: branchCompanyId.value
    })
    list.value = monthArr.value.reduce((arr, data, index) => {
      const obj = content.find((v) => v.month === index + 1) || {}
      arr.push({
        month: data,
        income: obj.intoAmount || 0,
        expend: obj.outAmount || 0
      })
      return arr
    }, [])
    const option = _myChart.getOption()
    option.series[0].data = list.value.map((v) => v.income)
    option.series[1].data = list.value.map((v) => v.expend)
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取年度信息失败')
  } finally {
    loading.value = false
  }
}
</script>

<style lang="scss" scoped></style>
