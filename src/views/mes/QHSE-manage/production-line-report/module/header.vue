<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
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
          style="width: 240px"
          class="filter-item"
          @change="handleDateChange"
        />
        <factory-select
          v-model="query.factoryId"
          class="filter-item"
          placeholder="可选择工厂"
          clearable
          style="width: 200px"
          @change="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import moment from 'moment'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import factorySelect from '@comp-base/factory-select.vue'

const defaultQuery = {
  date: [moment().startOf('month').valueOf(), moment().valueOf()],
  startDate: moment().startOf('month').valueOf(),
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
