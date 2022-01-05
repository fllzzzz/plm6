<template>
  <!--表格渲染-->
  <el-table-column
    v-if="columns.visible('name')"
    key="name"
    prop="name"
    sortable="custom"
    :show-overflow-tooltip="true"
    label="名称"
    min-width="120px"
  >
    <template v-slot="scope">
      <span>{{ scope.row.name }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="columns.visible('serialNumber')"
    key="serialNumber"
    prop="serialNumber"
    sortable="custom"
    :show-overflow-tooltip="true"
    label="编号"
    min-width="140px"
  >
    <template v-slot="scope">
      <span>{{ scope.row.serialNumber }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="columns.visible('material')"
    key="material"
    prop="material"
    sortable="custom"
    :show-overflow-tooltip="true"
    label="材质"
    min-width="80px"
  >
    <template v-slot="scope">
      <span>{{ scope.row.material }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="columns.visible('specification')"
    key="specification"
    prop="specification"
    sortable="custom"
    :show-overflow-tooltip="true"
    label="规格"
    min-width="140px"
  >
    <template v-slot="scope">
      <span>{{ scope.row.specification }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps } from 'vue'

defineProps({
  columns: {
    type: Object
  }
})
</script>
