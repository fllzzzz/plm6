<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <el-date-picker
          v-model="query.dateTime"
          type="month"
          size="small"
          class="date-item filter-item"
          style="width: 130px !important"
          placeholder="选择月"
          format="YYYY-MM"
          value-format="x"
          @change="crud.toQuery"
        />
        <!-- <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" /> -->
      </div>
    </template>
    <template #viewLeft>
      <slot name="viewLeft"></slot>
    </template>
  </crudOperation>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import moment from 'moment'

const defaultTime = moment().startOf('month').valueOf().toString()

const defaultQuery = {
  projectId: undefined,
  dateTime: defaultTime.toString()
}

const { crud, query } = regHeader(defaultQuery)
</script>
