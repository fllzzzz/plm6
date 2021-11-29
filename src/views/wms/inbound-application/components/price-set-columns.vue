<template>
  <el-table-column prop="unitPrice" align="center" width="115px" label="单价">
    <template #default="{ row }">
      <el-input-number
        v-model="row.unitPrice"
        :min="0"
        :max="9999999999"
        :controls="false"
        :step="5"
        :precision="2"
        size="mini"
        placeholder="单价"
        @change="handleUnitPriceChange($event, row)"
      />
    </template>
  </el-table-column>
  <el-table-column prop="amount" align="center" width="135px" label="金额">
    <template #default="{ row }">
      <el-input-number
        v-model="row.amount"
        :min="0"
        :max="9999999999"
        :controls="false"
        :step="5"
        :precision="2"
        size="mini"
        placeholder="金额"
        @change="handleAmountChange($event, row)"
      />
    </template>
  </el-table-column>
  <el-table-column v-if="requisitionsSNOptions" prop="purchaseId" label="申购单" min-width="130" align="center">
    <template #default="{ row }">
      <common-select
        v-model="row.requisitionsSN"
        :options="requisitionsSNOptions"
        :dataStructure="{ key: 'serialNumber', label: 'serialNumber', value: 'serialNumber' }"
        :extra-val="dittos.get('type')"
        show-extra
        type="other"
        placeholder="申购单"
      />
    </template>
  </el-table-column>
</template>

<script setup>
import { defineEmits, computed, inject } from 'vue'
import { regExtra } from '@/composables/form/use-form'
import { isNotBlank, toFixed } from '@/utils/data-type'

const emit = defineEmits(['amount-change'])
const { cu } = regExtra() // 表单
const dittos = inject('dittos')

// 申购单选择
const requisitionsSNOptions = computed(() => {
  if (isNotBlank(cu.props.order) && isNotBlank(cu.props.order.requisitionsSN)) {
    return cu.props.order.requisitionsSN.map((v) => {
      return { serialNumber: v }
    })
  } else {
    return null
  }
})

// 处理单价变化
function handleUnitPriceChange(val, row) {
  row.amount = toFixed(val * row.mete, 2, { toNum: true })
  emit('amount-change')
}

// 处理金额变化
function handleAmountChange(val, row) {
  row.unitPrice = toFixed(val / row.mete, 2, { toNum: true })
  emit('amount-change')
}
</script>
