<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.type"
      :options="TechnologyTypeEnum.ENUM"
      type="enum"
      :disabled-val="[TechnologyTypeEnum.STRUCTURE.V]"
      class="filter-item"
      @change="crud.toQuery"
    />
  </div>
  <crudOperation v-if="query.type===TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V"/>
</template>

<script setup>
import { TechnologyTypeEnum } from '@enum-ms/contract'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  type: TechnologyTypeEnum.SANDWICH_BOARD.V
}

const { crud, query } = regHeader(defaultQuery)
</script>
