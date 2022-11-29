<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <monomer-select-area-select
          v-model:monomerId="query.monomerId"
          v-model:areaId="query.areaId"
          clearable
          areaClearable
          :project-id="projectId"
          :monomerDisabled="!projectId"
          :areaDisabled="!projectId"
          @change="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'

const defaultQuery = {
}

const { crud, query } = regHeader(defaultQuery)
const projectId = useGlobalProjectIdChangeToQuery(crud)
</script>
