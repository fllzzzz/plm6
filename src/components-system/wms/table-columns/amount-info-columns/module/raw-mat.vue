<template>
  <el-table-column
    v-if="showInvoiceType"
    key="invoiceType"
    prop="invoiceType"
    label="票据类型"
    align="center"
    min-width="70px"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <span v-parse-enum="{ e: invoiceTypeEnum, v: row.invoiceType, f: 'SL' }" v-empty-text />
    </template>
  </el-table-column>
  <el-table-column v-if="showTaxRate" key="taxRate" prop="taxRate" label="税率" align="center" min-width="70" show-overflow-tooltip>
    <template #default="{ row }">
      <span v-empty-text="{ val: row.taxRate ? `${row.taxRate}%` : undefined }" />
    </template>
  </el-table-column>
  <el-table-column
    v-if="showUnitPrice"
    key="unitPrice"
    prop="unitPrice"
    label="含税单价"
    align="right"
    min-width="90px"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <span v-thousand="row.unitPrice" v-empty-text />
    </template>
  </el-table-column>
  <el-table-column v-if="showAmount" key="amount" prop="amount" label="金额" align="right" min-width="105px" show-overflow-tooltip>
    <template #default="{ row }">
      <span v-thousand="row.amount" v-empty-text />
    </template>
  </el-table-column>
  <el-table-column
    v-if="showAmountExcludingVAT"
    key="amountExcludingVAT"
    prop="amountExcludingVAT"
    label="不含税金额"
    align="right"
    min-width="105px"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <span v-thousand="row.amountExcludingVAT" v-empty-text />
    </template>
  </el-table-column>
  <el-table-column v-if="showInputVAT" key="inputVAT" prop="inputVAT" label="进项税" align="right" min-width="90px" show-overflow-tooltip>
    <template #default="{ row }">
      <span v-thousand="row.inputVAT" v-empty-text />
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
