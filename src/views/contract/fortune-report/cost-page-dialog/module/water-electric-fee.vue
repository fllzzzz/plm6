<template>
  <div class="app-container">
    <div class="head-container" style="display: flex; justify-content: space-between">
      <div style="width: 300px">
        <print-table
          v-permission="permission.print"
          api-key="waterElectricFee"
          :params="{projectId: props.costTypeData.projectId}"
          size="mini"
          type="warning"
          class="filter-item"
        />
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
      :summary-method="getSummaries"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column prop="type" key="type" label="类别" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.type }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="amount" key="amount" label="摊销总额（元）" align="center">
        <template v-slot="scope">
          <span>{{ toThousand(scope.row.amount, decimalPrecision.contract) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="mete" key="mete" label="累计产量（吨）" align="center">
        <template v-slot="scope">
          <span>{{ toThousand(scope.row.mete, 2) }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="avgPrice" key="avgPrice" label="平均单价（元/吨）" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.avgPrice?.toFixed(decimalPrecision.contract) }}</span>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>
<script setup>
// import { workOrderTypeEnum } from '@enum-ms/mes'
import { getWaterElectricList } from '@/api/contract/fortune-report/detail-fee'
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

watch(
  () => props.costTypeData?.projectId,
  (val) => {
    fetchWaterElectricFee()
  },
  { immediate: true, deep: true }
)

async function fetchWaterElectricFee() {
  try {
    const { content } = await getWaterElectricList({ projectId: props.costTypeData?.projectId })
    detailData.value = content || []
  } catch (error) {
    console.log('获取水电费失败', error)
  }
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['amount', decimalPrecision.value.contract], 'mete'],
    toThousandFields: ['amount', 'mete']
  })
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
