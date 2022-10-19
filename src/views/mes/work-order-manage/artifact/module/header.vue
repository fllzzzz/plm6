<template>
  <div v-show="crud.searchToggle">
    <!-- <common-radio-button
      v-model="query.productType"
      :options="componentTypeEnum.ENUM"
      type="enum"
      :unshowVal="[componentTypeEnum.ENCLOSURE.V, componentTypeEnum.AUXILIARY_MATERIAL.V, componentTypeEnum.MACHINE_PART.V]"
      default
      class="filter-item"
      @change="crud.toQuery"
    />
    <rrOperation /> -->
  </div>
  <crudOperation />
</template>

<script setup>
import { componentTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
// import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  productType: componentTypeEnum.ARTIFACT.V
}

const { crud, query } = regHeader(defaultQuery)
</script>
