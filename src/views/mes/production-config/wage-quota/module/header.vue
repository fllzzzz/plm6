<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.organizationType"
      :options="teamAttributeEnum.ENUM"
      class="filter-item"
      default
      type="enum"
      size="small"
      @change="crud.toQuery"
    />
    <component-radio-button
      v-model="query.productType"
      :options="typeEnum.ENUM"
      class="filter-item"
      type="enum"
      size="small"
      @change="crud.toQuery"
    />
    <rrOperation />
  </div>
  <crudOperation>
    <template #optLeft>
      <slot name="optLeft"></slot>
    </template>
  </crudOperation>
</template>

<script setup>
import { processMaterialListTypeEnum as typeEnum, teamAttributeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  productType: typeEnum.ARTIFACT.V,
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
