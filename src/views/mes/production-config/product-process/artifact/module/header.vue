<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.productionLineType"
      :options="artifactProductLineEnum.ENUM"
      show-option-all
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
    <!-- <rrOperation /> -->
  </div>
</template>

<script setup>
import { artifactProductLineEnum } from '@enum-ms/mes'
import { regHeader } from '@compos/use-crud'
// import rrOperation from '@crud/RR.operation'

const defaultQuery = {}

const { crud, query } = regHeader(defaultQuery)
</script>
