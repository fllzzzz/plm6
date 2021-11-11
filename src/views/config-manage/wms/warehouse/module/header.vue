<template>
  <div class="head-container">
    <factory-tabs v-model="query.factoryId" @tab-click="handleTabClick" />
    <crudOperation :disabled="!query.factoryId" />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation.vue'
import factoryTabs from '@comp-base/factory-tabs.vue'
const defaultQuery = {
  factoryId: undefined // 工厂
}

const { crud, query } = regHeader(defaultQuery)

function handleTabClick(data) {
  crud.props.factory = data
  crud.toQuery()
}

</script>
