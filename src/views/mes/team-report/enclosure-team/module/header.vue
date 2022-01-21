<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <el-date-picker
      v-model="query.date"
      type="daterange"
      range-separator=":"
      size="small"
      class="filter-item date-item"
      start-placeholder="开始日期"
      end-placeholder="结束日期"
      style="width: 240px"
      :clearable="false"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      value-format="x"
      @change="handleDateChange"
    />
    <factory-select v-model="query.factoryId" clearable class="filter-item" style="width: 200px" @change="crud.toQuery" />
    <rrOperation />
  </div>
  <crudOperation>
    <template #optRight>
      <print-table
        v-permission="crud.permission.print"
        api-key="mesEnclosureProductionLine"
        :params="{ ...query }"
        :before-print="handleBeforePrint"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </template>
  </crudOperation>
</template>

<script setup>
import moment from 'moment'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import factorySelect from '@comp-base/factory-select'
import { isBlank } from '@data-type/index'
import { ElMessage } from 'element-plus'

const defaultQuery = {
  date: [moment().startOf('month').valueOf(), moment().valueOf()],
  startDate: moment().startOf('month').valueOf(),
  endDate: moment().valueOf()
}

const { crud, query } = regHeader(defaultQuery)

function handleBeforePrint() {
  if (isBlank(crud.selections)) {
    ElMessage.warning('至少选择一条需要打印的生产线信息')
    return false
  }
}

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
