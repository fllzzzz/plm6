<template>
  <el-table-column
    v-if="showInvoiceType"
    key="invoiceType"
    prop="invoiceType"
    label="票据类型"
    align="center"
    min-width="70px"
    show-overflow-tooltip
  />
  <el-table-column v-if="showTaxRate" key="taxRate" prop="taxRate" label="税率" align="center" min-width="70" show-overflow-tooltip />
  <el-table-column
    v-if="showUnitPrice"
    key="unitPrice"
    prop="unitPrice"
    label="含税单价"
    align="right"
    min-width="90px"
    show-overflow-tooltip
  />
  <el-table-column v-if="showAmount" key="amount" prop="amount" label="含税金额" align="right" min-width="105px" show-overflow-tooltip />
  <el-table-column
    v-if="showUnitPriceExcludingVAT"
    key="unitPriceExcludingVAT"
    prop="unitPriceExcludingVAT"
    label="不含税单价"
    align="right"
    min-width="90px"
    show-overflow-tooltip
  />
  <el-table-column
    v-if="showAmountExcludingVAT"
    key="amountExcludingVAT"
    prop="amountExcludingVAT"
    label="不含税金额"
    align="right"
    min-width="105px"
    show-overflow-tooltip
  />
  <el-table-column v-if="showInputVAT" key="inputVAT" prop="inputVAT" label="进项税" align="right" min-width="90px" show-overflow-tooltip />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
  // 用于crud组件的列显隐
  columns: {
    type: Object
  },
  showUnitPriceE: {
    type: Boolean,
    default: false
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
  },
  // 显示进项税
  showInputVAT: {
    type: Boolean,
    default: true
  },
  // 显示金额
  showAmount: {
    type: Boolean,
    default: true
  },
  // 显示不含税金额
  showAmountExcludingVAT: {
    type: Boolean,
    default: true
  }
})

const showInvoiceType = computed(() => props.showInvoiceType && (isBlank(props.columns) || props.columns.visible('invoiceType')))
const showTaxRate = computed(() => props.showTaxRate && (isBlank(props.columns) || props.columns.visible('taxRate')))

const showUnitPrice = computed(() => props.showAmount && (isBlank(props.columns) || props.columns.visible('unitPrice')))
const showUnitPriceExcludingVAT = computed(
  () => props.showAmountExcludingVAT && (props.showUnitPriceE && (isBlank(props.columns) || props.columns.visible('unitPriceExcludingVAT')))
)
const showAmount = computed(() => props.showAmount && (isBlank(props.columns) || props.columns.visible('amount')))
const showAmountExcludingVAT = computed(() => props.showAmountExcludingVAT && (isBlank(props.columns) || props.columns.visible('amountExcludingVAT')))
const showInputVAT = computed(() => props.showInputVAT && (isBlank(props.columns) || props.columns.visible('inputVAT')))
</script>
