<template>
  <div v-show="crud.searchToggle">
  </div>
</template>

<script setup>

import { regHeader } from '@compos/use-crud'

const defaultQuery = {

}

const { crud } = regHeader(defaultQuery)
</script>
