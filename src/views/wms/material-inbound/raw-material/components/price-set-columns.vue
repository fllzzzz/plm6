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
        :precision="2"
        placeholder="金额"
        @change="handleAmountChange($event, row)"
      />
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps } from 'vue'
import usePriceSet from '@/composables/wms/use-price-set'

const props = defineProps({
  weightAttribute: { // 重量属性
    type: String,
    default: 'weighingTotalWeight'
  }
})

const { handleUnitPriceChange, handleAmountChange } = usePriceSet(props.weightAttribute)
</script>
