<template>
  <div v-show="crud.searchToggle">
    <el-date-picker
      v-model="query.dateTime"
      type="month"
      size="small"
      class="filter-item"
      placeholder="选择月"
      clearable
      value-format="x"
      style="width: 150px"
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.issueStatusEnum"
      :options="nestingStatusEnum.ENUM"
      show-option-all
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
    <!-- <rrOperation /> -->
  </div>
  <crudOperation>
    <template #optRight>
      <slot name="optRight" />
    </template>
  </crudOperation>
</template>

<script setup>
import { machinePartNestingStatusEnum as nestingStatusEnum } from '@enum-ms/mes'
import moment from 'moment'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
// import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  dateTime: moment().startOf('month').valueOf().toString()
}

const { crud, query } = regHeader(defaultQuery)
</script>
