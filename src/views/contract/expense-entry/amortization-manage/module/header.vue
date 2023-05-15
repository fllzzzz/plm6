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
          :default-time="defaultTime"
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
import { ref } from 'vue'

import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  date: [],
  startDate: undefined,
  endDate: undefined,
  ids: [] // 摊销分类的ids
}
const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

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
