
<template>
  <div class="app-container">
    <div class="head-container" style="display: flex; justify-content: space-between">
      <div style="width: 300px">
        <print-table v-permission="permission.print" api-key="subcontractFee" :params="{ projectId: props.costTypeData.projectId }" size="mini" type="warning" class="filter-item" />
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
      :data-format="dataFormat"
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column prop="subcontractClassName" key="subcontractClassName" label="科目" align="center" />
      <el-table-column prop="amount" key="amount" label="累计支付总额（元）" align="center" />
    </common-table>
  </div>
</template>
<script setup>
import { getSubcontractList } from '@/api/contract/fortune-report/detail-fee'
import { ref, defineProps, watch } from 'vue'

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

const dataFormat = ref([
  ['amount', ['to-thousand', decimalPrecision.contract]]
])

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
    props: [['amount', decimalPrecision.contract]],
    toThousandFields: ['amount']
  })
}

async function fetchList() {
  try {
    const { content } = await getSubcontractList({ projectId: props.costTypeData.projectId })
    detailData.value = content || []
  } catch (error) {
    console.log('分包费用', error)
  }
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
