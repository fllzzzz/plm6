<template>
  <el-table-column v-if="showUnitPrice" prop="unitPrice" label="含税单价" align="right" width="90px">
    <template #default="{ row }">
      <span v-empty-text v-thousand>{{ row.unitPrice }}</span>
    </template>
  </el-table-column>
  <el-table-column  v-if="showAmount" prop="amount" label="金额" align="right" width="105px">
    <template #default="{ row }">
      <span v-empty-text v-thousand>{{ row.amount }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showAmountExcludingVAT" prop="amountExcludingVAT" label="不含税金额" align="right" width="105px">
    <template #default="{ row }">
      <span v-empty-text v-thousand>{{ row.amountExcludingVAT }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showInputVAT" prop="inputVAT" label="进项税" align="right" width="90px">
    <template #default="{ row }">
      <span v-empty-text v-thousand>{{ row.inputVAT }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
  columns: { // 用于crud组件的列显隐
    type: Object
  }
})

const showUnitPrice = computed(() => isBlank(props.columns) || props.columns.visible('unitPrice'))
const showAmount = computed(() => isBlank(props.columns) || props.columns.visible('amount'))
const showAmountExcludingVAT = computed(() => isBlank(props.columns) || props.columns.visible('amountExcludingVAT'))
const showInputVAT = computed(() => isBlank(props.columns) || props.columns.visible('inputVAT'))

</script>
