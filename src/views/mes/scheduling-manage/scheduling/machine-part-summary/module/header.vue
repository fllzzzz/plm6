<template>
  <crudOperation>
    <template v-slot:optRight>
      <div v-show="crud.searchToggle">
        <monomer-select-area-select
          v-model:monomerId="query.monomerId"
          v-model:areaId="query.areaIds"
          :productType="productType"
          needConvert
          clearable
          monomerDefault
          areaDefault
          areaMultiple
          :project-id="projectId"
          @change="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { inject } from 'vue'

import { regHeader } from '@compos/use-crud'

import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'

import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {}

const { crud, query } = regHeader(defaultQuery)
const projectId = useGlobalProjectIdChangeToQuery(crud)

const productType = inject('productType')
</script>
