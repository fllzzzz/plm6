<template>
  <el-table-column
    v-if="showUnitPrice"
    :key="`${field}.unitPrice`"
    :prop="`${field}.unitPrice`"
    label="含税单价"
    align="right"
    width="90px"
    show-overflow-tooltip
  />
  <el-table-column
    v-if="showAmount"
    :key="`${field}.amount`"
    :prop="`${field}.amount`"
    label="金额"
    align="right"
    width="105px"
    show-overflow-tooltip
  />
  <el-table-column
    v-if="showAmountExcludingVAT"
    :key="`${field}.amountExcludingVAT`"
    :prop="`${field}.amountExcludingVAT`"
    label="不含税金额"
    align="right"
    width="105px"
    show-overflow-tooltip
  />
  <el-table-column
    v-if="showInputVAT"
    :key="`${field}.inputVAT`"
    :prop="`${field}.inputVAT`"
    label="进项税"
    align="right"
    width="90px"
    show-overflow-tooltip
  />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
  columns: {
    // 用于crud组件的列显隐
    type: Object
  },
  field: {
    // 字段
    type: String,
    default: 'material'
  }
})

const showUnitPrice = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.unitPrice`))
const showAmount = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.amount`))
const showAmountExcludingVAT = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.amountExcludingVAT`))
const showInputVAT = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.inputVAT`))
</script>
