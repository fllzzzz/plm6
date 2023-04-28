<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-input
        v-model.trim="query.name"
        size="small"
        placeholder="输入材质"
        style="width: 200px;"
        class="filter-item"
        clearable
      />
      <el-input
        v-model.trim="query.specification"
        size="small"
        placeholder="输入规格"
        style="width: 200px;"
        class="filter-item"
        clearable
      />
      <rrOperation/>
    </div>
    <crudOperation>
      <template #viewLeft>
        <!-- <print-table
          v-permission="crud.permission.print"
          api-key="saleOrderTracking"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        /> -->
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { inject, nextTick, watch } from 'vue'
import moment from 'moment'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const searchQuery = inject('searchQuery')

const defaultQuery = {
  startDate: undefined, endDate: undefined,
  name: undefined,
  specification: undefined,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

watch(
  searchQuery,
  (val) => {
    nextTick(() => {
      crud.query.projectId = val.projectId
      if (searchQuery.date && searchQuery.date.length > 1) {
        query.startDate = moment(searchQuery.date[0]).valueOf()
        query.endDate = moment(searchQuery.date[1]).valueOf()
      } else {
        query.startDate = undefined
        query.endDate = undefined
      }
      crud.toQuery()
    })
  },
  { immediate: true }
)
</script>
