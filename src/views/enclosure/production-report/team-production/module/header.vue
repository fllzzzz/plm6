<template>
  <div>
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <project-radio-button size="small" v-model="query.projectId" :type="'all'" class="filter-item" @change="crud.toQuery" />
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
            style="width: 240px; margin-right: 10px"
            class="filter-item date-item"
            @change="handleDateChange"
          />
          <rrOperation />
        </div>
      </template>
      <template #viewLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="enclosureTeamProduction"
          :params="{ ...query }"
          size="mini"
          type="warning"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import moment from 'moment'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  projectId: { value: undefined, resetAble: false },
  date: [moment().subtract(1, 'month').valueOf(), moment().valueOf()],
  startTime: moment().subtract(1, 'month').valueOf(),
  endTime: moment().valueOf()
}
const { crud, query } = regHeader(defaultQuery)

function handleDateChange(date = []) {
  if (date?.length) {
    query.startTime = date[0]
    query.endTime = date[1]
  } else {
    query.startTime = undefined
    query.endTime = undefined
  }
  crud.toQuery()
}
</script>
