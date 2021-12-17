<template>
  <el-table-column v-if="showInvoiceType" prop="invoiceType" label="票据类型" align="center" min-width="70px">
    <template #default="{ row }">
      <span v-parse-enum="{ e: invoiceTypeEnum, v: row.invoiceType, f:'SL' }" />
    </template>
  </el-table-column>
  <el-table-column v-if="showTaxRate" prop="taxRate" label="税率" align="center"  min-width="70">
    <template #default="{ row }">
      <span v-empty-text>{{ row.taxRate ? `${row.taxRate}%` : undefined }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showUnitPrice" prop="unitPrice" label="含税单价" align="right"  min-width="90px">
    <template #default="{ row }">
      <span v-empty-text v-thousand>{{ row.unitPrice }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showAmount" prop="amount" label="金额" align="right"  min-width="105px">
    <template #default="{ row }">
      <span v-empty-text v-thousand>{{ row.amount }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showAmountExcludingVAT" prop="amountExcludingVAT" label="不含税金额" align="right"  min-width="105px">
    <template #default="{ row }">
      <span v-empty-text v-thousand>{{ row.amountExcludingVAT }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showInputVAT" prop="inputVAT" label="进项税" align="right"  min-width="90px">
    <template #default="{ row }">
      <span v-empty-text v-thousand>{{ row.inputVAT }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'

const props = defineProps({
  // 用于crud组件的列显隐
  columns: {
    type: Object
  },
  // 显示票据类型
  showInvoiceType: {
    type: Boolean,
    default: false
  },
  // 显示税率
  showTaxRate: {
    type: Boolean,
    default: false
  }
})

const showInvoiceType = computed(() => props.showInvoiceType && (isBlank(props.columns) || props.columns.visible('invoiceType')))
const showTaxRate = computed(() => props.showTaxRate && (isBlank(props.columns) || props.columns.visible('taxRate')))

const showUnitPrice = computed(() => isBlank(props.columns) || props.columns.visible('unitPrice'))
const showAmount = computed(() => isBlank(props.columns) || props.columns.visible('amount'))
const showAmountExcludingVAT = computed(() => isBlank(props.columns) || props.columns.visible('amountExcludingVAT'))
const showInputVAT = computed(() => isBlank(props.columns) || props.columns.visible('inputVAT'))
</script>
