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
    <common-radio-button
      v-model="query.sequenceType"
      :options="typeEnum.ENUM"
      class="filter-item"
      type="enum"
      size="small"
      @change="productTypeChange"
    />
    <common-radio-button
      v-if="query.sequenceType === typeEnum.ARTIFACT.V"
      v-model="query.type"
      :options="processTypeEnum.ENUM"
      size="small"
      class="filter-item"
      type="enum"
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
import {
  processTypeEnum,
  processMaterialListTypeEnum as typeEnum,
  teamAttributeEnum
} from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  type: processTypeEnum.ONCE.V,
  sequenceType: typeEnum.ARTIFACT.V,
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)

function productTypeChange() {
  if (query.sequenceType === typeEnum.ARTIFACT.V) {
    query.type = processTypeEnum.ONCE.V
  } else {
    delete query.type
  }
  crud.toQuery()
}
</script>
