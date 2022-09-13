<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.status"
        :options="scheduleStatusEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.year"
        type="year"
        format="YYYY"
        class="filter-item"
        value-format="YYYY"
        placeholder="请选择"
        @change="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import { scheduleStatusEnum } from '@enum-ms/mes'

const defaultQuery = {
  status: undefined,
  year: undefined
}
const { crud, query } = regHeader(defaultQuery)

</script>
