<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <project-cascader v-model="query.projectId" clearable class="filter-item" style="width: 300px" @change="crud.toQuery" />
          <common-radio-button
            v-model="query.type"
            :options="packTypeEnum.ENUM"
            showOptionAll
            type="enumSL"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
          <common-radio-button
            v-model="query.status"
            :options="reviewStatusEnum.ENUM"
            showOptionAll
            type="enum"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
          <rrOperation />
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { packTypeEnum } from '@enum-ms/mes'
import { reviewStatusEnum } from '@enum-ms/common'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import projectCascader from '@comp-base/project-cascader.vue'

const defaultQuery = {
  type: undefined, status: undefined,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)
</script>
