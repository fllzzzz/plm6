<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.taskTypeEnum"
      :options="[componentTypeEnum.ARTIFACT, componentTypeEnum.ASSEMBLE]"
      type="enum"
      default
      class="filter-item"
      @change="crud.toQuery"
    />
    <monomer-select-area-select
      v-model:monomerId="query.monomerId"
      v-model:areaId="query.areaId"
      :productType="query.taskTypeEnum"
      :project-id="query.projectId"
      clearable
      areaClearable
    />
  </div>
</template>

<script setup>
import { inject, watch } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'

const projectId = inject('projectId')

const defaultQuery = {
  monomerId: undefined,
  areaId: undefined,
  taskTypeEnum: componentTypeEnum.ARTIFACT.V
}

const { crud, query, CRUD } = regHeader(defaultQuery)

CRUD.HOOK.beforeToQuery = () => {
  crud.query.projectId = projectId.value
}

watch([() => query.monomerId, () => query.areaId, () => projectId.value], () => {
  crud.toQuery()
})
</script>
