<template>
  <common-table
    ref="tableRef"
    :data="list"
    :max-height="maxHeight"
    :show-summary="false"
    :summary-method="getSummaries"
    :data-format="columnsDataFormat"
    :default-expand-all="false"
    highlight-current-row
    row-key="id"
  >
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <!-- 基础信息 -->
    <material-base-info-columns spec-merge fixed="left" :show-index="false" />
    <!-- 次要信息 -->
    <material-secondary-info-columns fixed="left" />
    <!-- 单位及其数量 TODO: 编辑 -->
    <material-unit-quantity-columns />
    <!-- 价格信息 -->
    <amount-info-columns :showAmountExcludingVAT="false" :showInputVAT="false" />
    <!-- 计量方式 -->
    <el-table-column
      v-if="materialType !== materialPurchaseClsEnum.MATERIAL.V"
      prop="weightMeasurementMode"
      label="计量方式"
      align="center"
      show-overflow-tooltip
    />
  </common-table>
</template>

<script setup>
import { defineProps } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { weightMeasurementModeEnum } from '@enum-ms/finance'
import { materialPurchaseClsEnum } from '@enum-ms/classification'
import amountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'

defineProps({
  list: {
    type: Array,
    default: () => []
  },
  maxHeight: {
    type: Number
  },
  materialType: {
    type: Number
  }
})

const columnsDataFormat = [['weightMeasurementMode', ['parse-enum', weightMeasurementModeEnum]]]

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['amount', 'mete'],
    toThousandFields: ['amount', 'mete']
  })
}
</script>

<style lang="scss" scoped></style>
