<template>
  <div class="header-container">
     <el-date-picker
        v-model="query.date"
        type="daterange"
        range-separator=":"
        size="small"
        value-format="x"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        unlink-panels
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width: 240px; margin-right: 10px;"
        class="filter-item date-item"
        @change="handleDateChange"
      />
    <project-radio-button size="small" v-model="query.projectId" :type="'all'" class="filter-item" @change="crud.toQuery" />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import moment from 'moment'

const defaultQuery = {
  projectId: undefined,
  date: [moment().subtract(1, 'week').valueOf(), moment().valueOf()],
  startDate: moment().subtract(1, 'week').valueOf(),
  endDate: moment().valueOf()
}

const { crud, query } = regHeader(defaultQuery)

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

<style>

</style>
