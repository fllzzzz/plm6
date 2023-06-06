<template>
  <div v-show="crud.searchToggle">
    <slot name="teamType"></slot>
    <process-select class="filter-item" :multiple="false" clearable v-model="query.processId" style="width: 200px" @change="crud.toQuery" />
    <rrOperation />
  </div>
  <crudOperation></crudOperation>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import processSelect from '@/components-system/bridge/process-select'

const defaultQuery = {
  processId: undefined,
  leaderName: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
