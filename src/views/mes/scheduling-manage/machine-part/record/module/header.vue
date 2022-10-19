<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.issueStatusEnum"
      :options="nestingStatusEnum.ENUM"
      show-option-all
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
    <rrOperation />
  </div>
  <crudOperation>
    <template #optRight>
      <slot name="optRight" />
    </template>
  </crudOperation>
</template>

<script setup>
import { machinePartNestingStatusEnum as nestingStatusEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {}

const { crud, query } = regHeader(defaultQuery)
</script>
