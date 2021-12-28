<template>
  <crudOperation>
    <template v-slot:optLeft>
      <div v-show="crud.searchToggle">
        <el-date-picker
          v-model="date"
          type="month"
          size="small"
          class="filter-item"
          placeholder="选择月"
          @change="crud.toQuery"
        />
        <common-radio-button
          v-model="query.processType"
          :options="processTypeEnum.ENUM"
          size="small"
          default
          class="filter-item"
          type="enum"
          @change="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { ref } from 'vue'

import { processTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import moment from 'moment'

const defaultQuery = {
  processType: processTypeEnum.ONCE.V
}

const date = ref(new Date())

const { crud, query, CRUD } = regHeader(defaultQuery)
useGlobalProjectIdChangeToQuery(crud)

CRUD.HOOK.beforeToQuery = () => {
  query.date = moment(date).valueOf()
}
</script>
