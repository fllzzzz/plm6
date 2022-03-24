<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
        <common-radio-button
          v-model="query.productType"
          :options="typeEnum"
          type="enum"
          size="small"
          class="filter-item"
          default
          @change="crud.toQuery"
        />
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
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import moment from 'moment'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { componentTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const typeEnum = {
  STRUCTURE: {
    L: '结构',
    K: 'STRUCTURE',
    V: componentTypeEnum.MACHINE_PART.V | componentTypeEnum.ASSEMBLE.V | componentTypeEnum.ARTIFACT.V
  },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: componentTypeEnum.ENCLOSURE.V }
}

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
