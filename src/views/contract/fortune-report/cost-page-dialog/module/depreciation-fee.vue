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
      show-summary
      :summary-method="getSummaries"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column prop="type" key="type" label="类别" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.type }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="totalAmount" key="totalAmount" label="摊销总额（元）" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.totalAmount }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="totalProduction" key="totalProduction" label="累计产量（吨）" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.totalProduction }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="averageUnitPrice" key="averageUnitPrice" label="平均单价（元/吨）" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.averageUnitPrice }}</span>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>
<script setup>
// import { workOrderTypeEnum } from '@enum-ms/mes'
import { ref, defineProps } from 'vue'
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

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [''],
    toThousandFields: ['']
  })
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
