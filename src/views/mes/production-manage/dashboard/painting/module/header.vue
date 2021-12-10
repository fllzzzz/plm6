<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <common-radio-button
          v-model="query.paintingType"
          :options="paintingTypeEnum.ENUM"
          type="enum"
          size="small"
          class="filter-item"
          @change="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { paintingTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  paintingType: paintingTypeEnum.PRIMER.V
}

const { crud, query } = regHeader(defaultQuery)
</script>
