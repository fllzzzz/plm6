
<template>
  <div class="app-container">
    <div class="head-container" style="display: flex; justify-content: space-between">
      <div style="width: 300px">
        <print-table v-permission="permission.print" api-key="testingFee" :params="{ projectId: props.costTypeData.projectId }" size="mini" type="warning" class="filter-item" />
      </div>
      <el-tag>合计（单位：元）：{{ toThousand(props.costTypeData?.amount,decimalPrecision.contract) }}</el-tag>
    </div>
    <common-table
      ref="tableRef"
      :data="detailData"
      :empty-text="'暂无数据'"
      :max-height="maxHeight"
      row-key="id"
      style="width: 100%"
      show-summary
      :data-format="dataFormat"
      :summary-method="getSummaries"
      :span-method="objectSpanMethod"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column prop="testingFeeTypeName" key="testingFeeTypeName" label="科目" align="center" />
      <el-table-column prop="amount" key="amount" label="总额" align="center" />
      <el-table-column prop="mete" key="mete" label="累计产量（吨）" align="center" />
      <el-table-column prop="averagePrice" key="averagePrice" label="平均单价（元/吨）" align="center"/>
    </common-table>
  </div>
</template>
<script setup>
import { getTestingList } from '@/api/contract/fortune-report/detail-fee'
import { ref, defineProps, watch, computed } from 'vue'

import { toThousand } from '@data-type/number'
import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const props = defineProps({
  costTypeData: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  }
})

const tableRef = ref()
const detailData = ref([])

const { maxHeight } = useMaxHeight({
  paginate: true
})

const dataFormat = computed(() => {
  return [
  // ['amount', ['to-thousand-ck', 'YUAN']],
    ['mete', 'to-thousand'],
    ['averagePrice', ['to-thousand', decimalPrecision.value.contract]]
  ]
})

watch(
  () => props.costTypeData.projectId,
  (value) => {
    fetchList()
  },
  { immediate: true, deep: true }
)

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['amount', decimalPrecision.value.contract]],
    toThousandFields: ['to-thousand']
  })
}

async function fetchList() {
  try {
    const { content = [] } = await getTestingList({ projectId: props.costTypeData.projectId })
    if (content.length > 0) {
      const values = content.map(v => Number(v.amount))
      let valuesSum = 0
      if (!values.every((value) => isNaN(value))) {
        valuesSum = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
      content[0].averagePrice = content[0].mete ? valuesSum / content[0].mete : valuesSum
    }
    detailData.value = content || []
  } catch (error) {
    console.log('检测费用', error)
  }
}

// 合计
function objectSpanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 3 || columnIndex === 4) {
    if (rowIndex === 0) {
      return {
        rowspan: detailData.value.length,
        colspan: 1
      }
    } else {
      return {
        rowspan: 0,
        colspan: 0
      }
    }
  }
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
