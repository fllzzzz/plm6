<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-input
        v-model.trim="query.supplierName"
        placeholder="车牌号"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
      />
      <el-input
        v-model.trim="query.supplierName"
        placeholder="编号"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
      />
      <el-input
        v-model.trim="query.serialNumber"
        placeholder="名称"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
      />
       <el-input
        v-model.trim="query.serialNumber"
        placeholder="办理人"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
      />
      <rrOperation />
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  date: undefined,
  startDate: undefined,
  endDate: undefined,
  serialNumber: undefined,
  supplierName: undefined,
  propertyType: 1
}
const { crud, query } = regHeader(defaultQuery)

</script>
