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
    <template #viewLeft>
      <div v-show="crud.searchToggle">
        <common-button v-permission="permission.manual.get" type="primary" size="mini" @click="manualVisible = true">
          手工填报
        </common-button>
      </div>
      <manual-filling v-model:visible="manualVisible" :project-id="query.projectId" />
    </template>
  </crudOperation>
</template>

<script setup>
import { ref, inject } from 'vue'

import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import crudOperation from '@crud/CRUD.operation'
// import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import manualFilling from './manual-filling'

const defaultQuery = {
  projectId: { value: undefined, resetAble: false }
}

const permission = inject('permission')
const manualVisible = ref(false)

const { crud, query } = regHeader(defaultQuery)
useGlobalProjectIdChangeToQuery(crud)
</script>

<style lang="scss" scoped>
.crud-opts {
  padding-top: 0;
}
</style>
