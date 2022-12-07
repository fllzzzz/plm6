<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.productionLineTypeEnum"
      :options="artifactProductLineEnum.ENUM"
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

const defaultQuery = {
  productionLineTypeEnum: artifactProductLineEnum.TRADITION.V
}

const { crud, query } = regHeader(defaultQuery)
</script>
