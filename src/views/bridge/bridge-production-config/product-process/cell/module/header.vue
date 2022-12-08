<template>
  <div v-show="crud.searchToggle">
    <crudOperation>
      <template #optRight>
        <common-radio-button
          v-model="query.boolSectionSteel"
          :options="extrusionClsEnum.ENUM"
          showOptionAll
          type="enum"
          class="filter-item"
          @change="crud.toQuery"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import { extrusionClsEnum } from '@enum-ms/classification'

const defaultQuery = {
  boolSectionSteel: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
