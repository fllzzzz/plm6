<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <common-radio-button
            v-model="query.basicClass"
            :options="classificationEnum.ENUM"
            showOptionAll
            :optionAllValue="undefined"
            type="enum"
            class="filter-item"
            @change="crud.toQuery"
          />
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
            placeholder="采购合同编号"
            class="filter-item"
            style="width: 200px;"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model="query.supplierName"
            placeholder="供应商"
            class="filter-item"
            style="width: 200px;"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <rrOperation/>
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import moment from 'moment'
import { classificationEnum } from '@enum-ms/classification'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  basicClass: undefined, date: undefined, startDate: undefined, endDate: undefined,
  serialNumber: undefined, supplierName: undefined
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
