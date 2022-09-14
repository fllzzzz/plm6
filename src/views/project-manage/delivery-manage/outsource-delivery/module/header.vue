<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <!-- <common-radio-button
        v-model="query.settlementStatus"
        :options="TechnologyMainTypeEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      /> -->
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
        v-model.trim="query.serialNumber"
        placeholder="车牌"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.supplierName"
        placeholder="司机"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.supplierName"
        placeholder="收货人"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.supplierName"
        placeholder="办理人"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import moment from 'moment'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
// import { TechnologyMainTypeEnum } from '@enum-ms/contract'

const defaultQuery = {
  date: undefined,
  startDate: undefined,
  endDate: undefined,
  serialNumber: undefined,
  supplierName: undefined,
  propertyType: 1
  // auditStatus: auditTypeEnum.AUDITING.V
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
