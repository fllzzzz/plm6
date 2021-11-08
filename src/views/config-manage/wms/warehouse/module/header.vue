<template>
  <div class="head-container">
    <factory-tabs v-model="query.factoryId" @tab-click="crud.toQuery()" />
    <crudOperation />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import factoryTabs from '@comp-common/factory-tabs/index.vue'
const defaultQuery = {
  factoryId: undefined // 工厂
}

const { crud, query } = regHeader(defaultQuery)

</script>
