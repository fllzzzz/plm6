<template>
  <div class="head-container" style="display: flex; justify-content: space-between">
    <div>
      <el-date-picker
        v-model="query.dateTime"
        type="month"
        size="small"
        class="date-item filter-item"
        style="width: 130px !important"
        placeholder="选择月"
        format="YYYY-MM"
        value-format="x"
        :disabled-date="disabledDate"
        @change="crud.toQuery"
      />
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    </div>

    <div style="width: 300px">
      <print-table :api-key="apiKey" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import moment from 'moment'

const defaultTime = moment().startOf('month').valueOf()

const defaultQuery = {
  projectId: undefined,
  dateTime: defaultTime.toString()
}

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

const { crud, query } = regHeader(defaultQuery)
</script>

<style>
</style>
