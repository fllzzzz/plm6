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
  />
  <el-table-column
    v-if="columns.visible('serialNumber')"
    key="serialNumber"
    prop="serialNumber"
    sortable="custom"
    :show-overflow-tooltip="true"
    label="编号"
    min-width="140px"
  />
  <el-table-column
    v-if="columns.visible('material')"
    key="material"
    prop="material"
    sortable="custom"
    :show-overflow-tooltip="true"
    label="规格"
    min-width="120px"
  />
</template>

<script setup>
import { defineProps } from 'vue'

defineProps({
  columns: {
    type: Object
  }
})
</script>
