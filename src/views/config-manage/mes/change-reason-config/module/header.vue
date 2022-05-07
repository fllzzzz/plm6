<template>
  <div>
    <div v-show="crud.searchToggle">
      <crudOperation />
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  name: undefined
}

const { crud } = regHeader(defaultQuery)
</script>
