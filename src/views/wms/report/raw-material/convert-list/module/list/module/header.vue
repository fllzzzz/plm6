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
        v-model.trim="query.receiptSerialNumber"
        placeholder="转换单号"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
      />
      <el-input
        v-model.trim="query.outSerialNumber"
        placeholder="出库单号"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
      />
      <!-- <project-cascader
        v-model="query.projectId"
        placeholder="所属项目"
        clearable
        @change="crud.toQuery"
        class="filter-item"
        style="width: 300px"
      /> -->
      <el-input
        v-model.trim="query.specification"
        placeholder="规格"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
      />
      <el-input
        v-model.trim="query.brand"
        placeholder="品牌"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import moment from 'moment'
import { reviewStatusEnum } from '@enum-ms/common'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  date: [],
  startDate: undefined,
  endDate: undefined,
  receiptSerialNumber: undefined,
  outSerialNumber: undefined,
  specification: undefined,
  brand: undefined,
  status: { value: reviewStatusEnum.PASS.V, resetAble: false },
  boolAll: { value: 1, resetAble: false }
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
