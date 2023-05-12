<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.productionLineTypeEnum"
      :options="hasIntelligent ? artifactProductLineEnum.ENUM : traditionLineEnum.ENUM"
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
    <!-- <rrOperation /> -->
  </div>
</template>

<script setup>
import { artifactProductLineEnum, traditionLineEnum } from '@enum-ms/mes'
import { regHeader } from '@compos/use-crud'
import { mapGetters } from '@/store/lib'
// import rrOperation from '@crud/RR.operation'

const { hasIntelligent } = mapGetters('hasIntelligent')
const defaultQuery = {
  productionLineTypeEnum: artifactProductLineEnum.TRADITION.V
}

const { crud, query } = regHeader(defaultQuery)
</script>
