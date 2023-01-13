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
      <el-table-column prop="itemType" key="itemType" label="物料种类" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.itemType }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="specification" key="specification" label="规格" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="accountingUnit" key="accountingUnit" label="核算单位" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.accountingUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="accountingMete" key="accountingMete" label="核算量" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.accountingMete }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="unitPrice" key="unitPrice" label="单价" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.unitPrice }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="totalPrice" key="totalPrice" label="总价" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.totalPrice }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="outboundDate" key="outboundDate" label="出库日期" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.outboundDate ? parseTime(scope.row.outboundDate) : '-' }}</span>
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
import { parseTime } from '@/utils/date'
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
