<template>
  <crudOperation />
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {}

regHeader(defaultQuery)
</script>
