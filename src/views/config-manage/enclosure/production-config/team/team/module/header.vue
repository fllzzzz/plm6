<template>
  <div v-show="crud.searchToggle">
    <slot name="teamType"></slot>
  </div>
  <crudOperation />
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const { crud } = regHeader({})
</script>
