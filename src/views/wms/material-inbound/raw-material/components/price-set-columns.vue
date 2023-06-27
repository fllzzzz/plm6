<template>
  <el-table-column prop="unitPrice" align="center" width="135px" label="含税单价">
    <template #default="{ row }">
      <common-input-number
        v-if="row"
        v-model="row.unitPrice"
        :min="0"
        :max="9999999999"
        :controls="false"
        :step="1"
        size="mini"
        placeholder="含税单价"
        @change="handleUnitPriceChange($event, row)"
      />
    </template>
  </el-table-column>
  <el-table-column prop="amount" align="center" width="135px" label="金额">
    <template #default="{ row }">
      <common-input-number
        v-if="row"
        v-model="row.amount"
        :min="0"
        :max="9999999999"
        :controls="false"
        :step="1"
        size="mini"
        :precision="decimalPrecision.wms"
        placeholder="金额"
        @change="handleAmountChange($event, row)"
      />
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps } from 'vue'

import { isNotBlank, toPrecision } from '@/utils/data-type'
import { getDP } from '@/utils/data-type/number'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const props = defineProps({
  weightAttribute: { // 重量属性
    type: String,
    default: 'weighingTotalWeight'
  }
})

// 处理含税单价变化
function handleUnitPriceChange(val, row) {
  const dp = getDP(val)
  if (dp > 10) {
    row.unitPrice = toPrecision(val, 10)
    val = row.unitPrice
  }
  row.amount = isNotBlank(val) ? toPrecision(val * row[props.weightAttribute], decimalPrecision.wms) : undefined
}

// 处理金额变化
function handleAmountChange(val, row) {
  row.unitPrice = isNotBlank(val) ? toPrecision(val / row[props.weightAttribute], 10) : undefined
}
</script>
