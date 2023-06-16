<template>
  <div class="app-container">
    <div class="head-container">
      <common-radio-button v-model="currentType" :options="typeEnum.ENUM" default type="enum" @change="fetchInfo" />
      <div style="float: right; font-size: 16px; color: #303133">单位：元</div>
    </div>
    <div v-loading="loading" class="view-summary">
      <panel name="应收项目" num-color="#1890ff" :end-val="summaryInfo.projectCount || 0" class="summary-item" />
      <panel
        v-if="currentType === typeEnum.INVOICE.V"
        name="开票总额"
        num-color="#1890ff"
        :end-val="summaryInfo.invoiceAmount || 0"
        class="summary-item"
      />
      <panel
        v-if="currentType === typeEnum.SETTLEMENT.V"
        name="结算总额"
        num-color="#1890ff"
        :end-val="summaryInfo.settlementAmount || 0"
        class="summary-item"
      />
      <panel name="实际收款" num-color="#1890ff" :end-val="summaryInfo.collectionAmount || 0" class="summary-item" />
      <panel name="应收款" num-color="#1890ff" :end-val="summaryInfo.receivableAmount || 0" class="summary-item" />
    </div>
    <div style="display: flex">
      <div v-loading="loading" :style="{ height: maxHeight + 'px' }" style="flex: 1; margin-right: 20px">
        <div id="pieChart" style="width: 100%; height: 100%"></div>
      </div>
      <common-table
        v-loading="loading"
        :height="maxHeight"
        :data="list"
        :data-format="dataFormat"
        show-summary
        :summary-method="getSummaries"
        style="flex: 1"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="project.shortName" :show-overflow-tooltip="true" label="所属项目" min-width="120">
          <template #default="{ row }">
            <span class="project-name">{{ projectNameFormatter(row.project, null, false) }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="contractAmount" :show-overflow-tooltip="true" label="合同额" align="center">
          <template #default="{ row }">
            <span>{{ row.contractAmount }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="currentType === typeEnum.INVOICE.V"
          prop="invoiceAmount"
          :show-overflow-tooltip="true"
          label="累计开票额"
          align="center"
        >
          <template #default="{ row }">
            <span>{{ row.invoiceAmount }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="currentType === typeEnum.SETTLEMENT.V"
          prop="settlementAmount"
          :show-overflow-tooltip="true"
          label="结算额"
          align="center"
        >
          <template #default="{ row }">
            <span>{{ row.settlementAmount }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="collectionAmount" :show-overflow-tooltip="true" label="实际收款" align="center">
          <template #default="{ row }">
            <span>{{ row.collectionAmount }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="receivableAmount" :show-overflow-tooltip="true" label="应收款" align="center">
          <template #default="{ row }">
            <span>{{ row.receivableAmount }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="账龄(天)" align="center">
          <template #default="{ row }">
            <span>{{ row.accountAge }}</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
  </div>
</template>

<script setup>
import { getCollectionAnalysis } from '@/api/operation/receivables'
import { ref } from 'vue'

import { projectNameFormatter } from '@/utils/project'
import { tableSummary } from '@/utils/el-extra'
import { constantize } from '@enum/base'

import useMaxHeight from '@compos/use-max-height'
import useChart from '@compos/use-chart'
import panel from '@/components/Panel'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container', '.view-summary']
})

const loading = ref(false)
const summaryInfo = ref({})
const currentType = ref()
const list = ref([])

const typeEnum = {
  INVOICE: { L: '已开票应收', K: 'INVOICE', V: 1, field: 'invoiceAmount' },
  SETTLEMENT: { L: '已结算应收', K: 'SETTLEMENT', V: 2, field: 'settlementAmount' }
}
constantize(typeEnum)

const dataFormat = ref([
  ['contractAmount', ['to-thousand', decimalPrecision.operation]],
  ['invoiceAmount', ['to-thousand', decimalPrecision.operation]],
  ['settlementAmount', ['to-thousand', decimalPrecision.operation]],
  ['collectionAmount', ['to-thousand', decimalPrecision.operation]],
  ['receivableAmount', ['to-thousand', decimalPrecision.operation]]
])

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['contractAmount', decimalPrecision.operation], ['invoiceAmount', decimalPrecision.operation], ['settlementAmount', decimalPrecision.operation], ['receivableAmount', decimalPrecision.operation]],
    toThousandFields: ['contractAmount', 'invoiceAmount', 'settlementAmount', 'receivableAmount']
  })
}

const { getMyChart } = useChart({
  elementId: 'pieChart',
  fetchHook: fetchInfo,
  initOption: {
    title: {
      text: typeEnum.VL[currentType.value]
    },
    xAxis: { show: false },
    tooltip: {
      trigger: 'item'
    },
    legend: { cancelDefaultData: true, top: 'center', left: 'right', orient: 'vertical' },
    series: [
      {
        type: 'pie',
        radius: [50, 110],
        center: ['40%', '50%'],
        roseType: 'area',
        itemStyle: {
          borderRadius: 8
        },
        label: {
          show: true
        },
        data: []
      }
    ]
  }
})

async function fetchInfo() {
  try {
    const _myChart = getMyChart()
    if (!_myChart) return
    const option = _myChart.getOption()
    loading.value = true
    const { content } = await getCollectionAnalysis({
      type: currentType.value
    })
    summaryInfo.value = content.reduce(
      (pre, data) => {
        pre.projectCount++
        pre.invoiceAmount += data.invoiceAmount || 0
        pre.collectionAmount += data.collectionAmount || 0
        pre.settlementAmount += data.settlementAmount || 0
        const age = data.accountAge
        if (age <= 10) {
          pre.accountAge[0].value++
        } else if (age > 10 && age <= 30) {
          pre.accountAge[1].value++
        } else if (age > 30 && age <= 60) {
          pre.accountAge[2].value++
        } else if (age > 60 && age <= 180) {
          pre.accountAge[3].value++
        } else if (age > 180) {
          pre.accountAge[4].value++
        }
        return pre
      },
      {
        projectCount: 0,
        invoiceAmount: 0,
        collectionAmount: 0,
        settlementAmount: 0,
        receivableAmount: 0,
        accountAge: [
          { value: 0, name: '≤10天' },
          { value: 0, name: '<10天≤30天' },
          { value: 0, name: '<30天≤60天' },
          { value: 0, name: '<60天≤180天' },
          { value: 0, name: '>180天' }
        ]
      }
    )
    summaryInfo.value.receivableAmount = summaryInfo.value[typeEnum.V[currentType.value].field] - summaryInfo.value.collectionAmount
    list.value = content.map((v) => {
      v.project = {
        id: v.id,
        name: v.name,
        serialNumber: v.serialNumber,
        shortName: v.shortName
      }
      const amount = v[typeEnum.V[currentType.value].field] || 0
      v.collectionAmount = v.collectionAmount || 0
      v.receivableAmount = amount - v.collectionAmount
      return v
    })
    option.title[0].text = typeEnum.VL[currentType.value]
    option.series[0].data = summaryInfo.value.accountAge
    _myChart.setOption(option)
  } catch (error) {
    console.log(error, '获取应收款信息')
  } finally {
    loading.value = false
  }
}
</script>

<style lang="scss" scoped>
.view-summary {
  display: flex;
  margin-bottom: 15px;

  .summary-item {
    flex: 1;

    &:not(:last-child) {
      margin-right: 10px;
    }
  }
}
</style>
