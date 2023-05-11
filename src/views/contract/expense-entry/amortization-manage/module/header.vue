<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
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
      </template>
      <template #viewLeft>
        <slot name="viewLeft" />
      </template>
    </crudOperation>
  </div>
</template>
<script setup>
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  date: [],
  startDate: undefined,
  endDate: undefined,
  ids: [] // 摊销分类的ids
}

const { crud, query } = regHeader(defaultQuery)

function handleDateChange() {
  if (query.date?.length) {
    query.startDate = query.date[0]
    query.endDate = query.date[1]
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}
</script>
