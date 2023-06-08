<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.id"
      :options="rivetWeldList"
      :dataStructure="{ key: 'id', label: 'typeName', value: 'id' }"
      default
      @change="crud.toQuery"
    ></common-radio-button>
  </div>
  <crudOperation />
</template>

<script setup>
import { inject } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {}

const rivetWeldList = inject('rivetWeldList')

const { crud, query } = regHeader(defaultQuery)
</script>
