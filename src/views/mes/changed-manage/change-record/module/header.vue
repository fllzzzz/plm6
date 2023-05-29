<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.status"
      :options="changeRecordStatusEnum.ENUM"
      show-option-all
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
    <project-cascader
      v-model="query.projectId"
      placeholder="变更项目"
      clearable
      @change="crud.toQuery"
      class="filter-item"
      style="width: 300px"
    />
    <monomer-select
      v-model="query.monomerId"
      :project-id="query.projectId"
      :default="false"
      clearable
      class="filter-item"
      @change="crud.toQuery"
    />
    <rrOperation />
  </div>
  <crudOperation />
</template>

<script setup>
import { changeRecordStatusEnum } from '@enum-ms/production'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'

const defaultQuery = {}

const { crud, query } = regHeader(defaultQuery)
</script>
