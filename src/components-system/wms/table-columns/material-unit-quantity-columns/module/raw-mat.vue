<template>
  <el-table-column prop="measureUnit" label="计量单位" align="center" width="70px">
    <template #default="{ row }">
      <span v-empty-text>{{ row.measureUnit }}</span>
    </template>
  </el-table-column>
  <el-table-column prop="number" label="数量" align="center" width="120px">
    <template #default="{ row }">
      <span v-empty-text v-to-fixed="row.measurePrecision">{{ row.number }}</span>
    </template>
  </el-table-column>
  <el-table-column prop="accountingUnit" label="核算单位" align="center" width="70px">
    <template #default="{ row }">
      <span v-empty-text>{{ row.accountingUnit }}</span>
    </template>
  </el-table-column>
  <el-table-column prop="mete" label="核算量" align="center" width="120px">
    <template #default="{ row }">
      <span v-empty-text v-to-fixed="row.accountingPrecision">{{ row.mete }}</span>
    </template>
  </el-table-column>
</template>

<script setup></script>
