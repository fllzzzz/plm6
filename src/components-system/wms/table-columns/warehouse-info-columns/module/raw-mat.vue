<template>
  <el-table-column prop="warehouse" label="仓库" align="left" min-width="90px">
    <template #default="{ row }">
      <span v-empty-text>{{ typeof row.warehouse === 'object' ? row.warehouse.name : row.warehouse }}</span>
    </template>
  </el-table-column>
</template>

