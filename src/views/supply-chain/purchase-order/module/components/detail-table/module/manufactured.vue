<template>
  <common-table
    ref="tableRef"
    :data="list"
    show-summary
    :summary-method="getSummaries"
    :data-format="columnsDataFormat"
    :max-height="maxHeight"
    highlight-current-row
    row-key="id"
  >
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="monomer.name" label="单体" align="center" show-overflow-tooltip min-width="120px" />
    <el-table-column prop="area.name" label="区域" align="center" show-overflow-tooltip min-width="120px" />
    <el-table-column prop="name" label="名称" align="center" show-overflow-tooltip min-width="100px" />
    <el-table-column prop="serialNumber" label="编号" align="center" show-overflow-tooltip min-width="100px" />
    <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip min-width="140px" />
    <el-table-column prop="length" label="长度（mm）" align="center" show-overflow-tooltip min-width="100px" />
    <el-table-column prop="material" label="材质" align="center" show-overflow-tooltip />
    <el-table-column prop="quantity" label="数量" align="center" show-overflow-tooltip />
    <el-table-column label="总重(kg)" align="center" show-overflow-tooltip>
      <template #default="{ row: { sourceRow: row } }">
        <span>{{ toPrecision(row.quantity * row.netWeight, 2) }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="pricingMethod" label="计价方式" align="center" show-overflow-tooltip />
    <!-- 价格信息 -->
    <amount-info-columns :showAmountExcludingVAT="false" :showInputVAT="false" />
    <el-table-column prop="destination" label="目的地" align="center" show-overflow-tooltip />
  </common-table>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { toPrecision } from '@/utils/data-type'
import { tableSummary } from '@/utils/el-extra'
import { wageQuotaTypeEnum } from '@enum-ms/mes'
import { destinationTypeEnum } from '@enum-ms/production'
import amountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'

defineProps({
  list: {
    type: Array,
    default: () => []
  },
  maxHeight: {
    type: Number
  }
})

const columnsDataFormat = ref([
  ['destination', ['parse-enum', destinationTypeEnum]],
  ['pricingMethod', ['parse-enum', wageQuotaTypeEnum]]
])

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['amount', 'mete'],
    toThousandFields: ['amount', 'mete']
  })
}
</script>

<style lang="scss" scoped></style>
