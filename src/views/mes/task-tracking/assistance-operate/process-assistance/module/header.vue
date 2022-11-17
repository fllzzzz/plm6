<template>
<div v-show='crud.searchToggle'>
<rrOperation />
</div>
<crudOperation/>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
}

const { crud, query } = regHeader(defaultQuery)
</script>
