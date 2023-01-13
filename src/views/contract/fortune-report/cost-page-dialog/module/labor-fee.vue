<template>
  <div class="app-container">
    <div class="head-container" style="display: flex; justify-content: space-between">
      <div style="width: 300px">
        <print-table :api-key="apiKey" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
      </div>
      <el-tag>合计（单位：元）：{{ toThousand(props.costTypeData?.amount) }}</el-tag>
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
      <el-table-column prop="name" key="name" label="工序" align="center" />
      <el-table-column prop="price" key="price" label="人工费总额（元）" align="center" />
      <el-table-column prop="mete" key="mete" label="累计产量（吨）" align="center" />
      <el-table-column prop="averageUnitPrice" key="averageUnitPrice" label="平均单价（元/吨）" align="center" />
    </common-table>
  </div>
</template>
<script setup>
import { getLaborFeeList } from '@/api/contract/fortune-report/detail-fee'
import { ref, defineProps, watch } from 'vue'

import { toThousand } from '@data-type/number'
import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'

const props = defineProps({
  costTypeData: {
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
  ['price', 'to-thousand'],
  ['mete', 'to-thousand'],
  ['averageUnitPrice', 'to-thousand']
])

watch(
  () => props.costTypeData.projectId,
  (value) => {
    fetchLaborFee()
  },
  { immediate: true, deep: true }
)

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [''],
    toThousandFields: ['']
  })
}

async function fetchLaborFee() {
  try {
    const { content } = await getLaborFeeList({ projectId: props.costTypeData.projectId })
    content.map(v => {
      v.averageUnitPrice = v.mete ? v.price / v.mete : 0
    })
    detailData.value = content || []
  } catch (error) {
    console.log('人工费用', error)
  }
}

</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
