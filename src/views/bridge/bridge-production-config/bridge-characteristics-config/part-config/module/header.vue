<template>
  <div>
    <div v-show="crud.searchToggle">
      <crudOperation>
        <template #optRight>
          <common-radio-button
            v-model="query.boolSectionSteel"
            :options="partClsEnum.ENUM"
            showOptionAll
            type="enum"
            class="filter-item"
            @change="crud.toQuery"
          />
        </template>
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import { partClsEnum } from '@enum-ms/bridge'

const defaultQuery = {
  boolSectionSteel: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
