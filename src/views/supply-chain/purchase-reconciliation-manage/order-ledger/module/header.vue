<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.date"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item date-item"
        start-placeholder="开始时间"
        end-placeholder="结束时间"
        style="width: 240px"
        @change="handleDateChange"
      />
      <el-input
        v-model="query.serialNumber"
        placeholder="采购单号"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <supplier-select
        v-model="query.supplierId"
        clearable
        class="filter-item"
        placeholder="可选择供应商搜索"
        show-hide
        style="width: 240px"
        @change="crud.toQuery"
      />
      <rrOperation />
    </div>
  </div>
</template>

<script setup>
import moment from 'moment'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'

import supplierSelect from '@comp-base/supplier-select/index.vue'

const defaultQuery = {
  date: undefined,
  startDate: undefined,
  endDate: undefined,
  serialNumber: undefined,
  supplierId: undefined
}
const { crud, query } = regHeader(defaultQuery)

// 时间变动
function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startDate = moment(query.date[0]).valueOf()
    query.endDate = moment(query.date[1]).valueOf()
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}
</script>
