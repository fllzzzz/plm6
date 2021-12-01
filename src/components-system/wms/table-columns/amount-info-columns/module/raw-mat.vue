<template>
  <el-table-column prop="unitPrice" label="含税单价" align="right" width="90px">
    <template #default="{ row }">
      <span v-empty-text v-thousand>{{ row.unitPrice }}</span>
    </template>
  </el-table-column>
  <el-table-column prop="amount" label="金额" align="right" width="105px">
    <template #default="{ row }">
      <span v-empty-text v-thousand>{{ row.amount }}</span>
    </template>
  </el-table-column>
  <el-table-column prop="priceExcludingVAT" label="不含税金额" align="right" width="105px">
    <template #default="{ row }">
      <span v-empty-text v-thousand>{{ row.priceExcludingVAT }}</span>
    </template>
  </el-table-column>
  <el-table-column prop="inputVAT" label="进项税" align="right" width="90px">
    <template #default="{ row }">
      <span v-empty-text v-thousand>{{ row.inputVAT }}</span>
    </template>
  </el-table-column>
</template>
