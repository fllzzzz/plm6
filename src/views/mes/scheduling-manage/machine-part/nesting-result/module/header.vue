<template>
  <div v-show="crud.searchToggle">
    <!-- <common-radio-button
      v-model="query.boolNestCutEnum"
      :options="layOffWayTypeEnum.ENUM"
      showOptionAll
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    /> -->
    <common-radio-button
      v-if="query.boolNestCutEnum === layOffWayTypeEnum.NESTING.V"
      v-model="query.issueStatusEnum"
      :options="issueStatusEnum.ENUM"
      :unshowVal="[issueStatusEnum.NOT_NESTING.V]"
      showOptionAll
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
    <rrOperation />
  </div>
  <!-- <crudOperation>
    <template #optRight>
      <slot name="optRight" />
    </template>
  </crudOperation> -->
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import { layOffWayTypeEnum } from '@enum-ms/uploading-form'
import { machinePartSchedulingIssueStatusEnum as issueStatusEnum } from '@enum-ms/mes'
// import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {}

// const { crud } = regHeader(defaultQuery)
const { crud, query } = regHeader(defaultQuery)
</script>
