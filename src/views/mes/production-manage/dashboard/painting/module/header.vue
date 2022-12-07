<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <monomer-select
          v-model="query.monomerId"
          :project-id="query.projectId"
          :default="false"
          clearable
          class="filter-item"
          @change="crud.toQuery"
        />
        <!-- <rrOperation /> -->
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import crudOperation from '@crud/CRUD.operation'
// import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'

const defaultQuery = {
  projectId: { value: undefined, resetAble: false }
}

const { crud, query } = regHeader(defaultQuery)
useGlobalProjectIdChangeToQuery(crud)
</script>
