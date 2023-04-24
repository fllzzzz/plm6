<template>
  <div class="head-container">
    <div style="float:left">
      <el-date-picker
        v-model="query.date"
        type="daterange"
        range-separator=":"
        size="small"
        value-format="x"
        class="filter-item date-item"
        start-placeholder="开始时间"
        end-placeholder="结束时间"
        style="width: 240px"
        @change="handleDateChange"
      />
      <common-radio-button
        v-model="query.auditStatus"
        :options="auditTypeEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
        style="float:left;"
      />
    </div>
    <common-button type="primary" style="float:right;" @click="crud.toAdd" v-permission="crud.permission.add">+添加</common-button>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import { auditTypeEnum } from '@enum-ms/contract'

const defaultQuery = {
  date: [],
  startDate: undefined,
  endDate: undefined,
  auditStatus: auditTypeEnum.AUDITING.V
}
const { crud, query } = regHeader(defaultQuery)

// 时间变动
function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startDate = query.date[0]
    query.endDate = query.date[1]
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}

</script>
