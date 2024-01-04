<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <common-radio-button
        v-model="query.projectWarehouseType"
        :options="projectWarehouseTypeEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="handleTypeChange"
      />
      <project-cascader
        v-if="query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V"
        v-model="query.projectId"
        placeholder="所属项目"
        clearable
        @change="crud.toQuery"
        class="filter-item"
        style="width: 300px"
      />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  projectWarehouseType: undefined,
  projectId: undefined
}

const { crud, query } = regHeader(defaultQuery)

function handleTypeChange(val) {
  if (val === projectWarehouseTypeEnum.PUBLIC.V) {
    query.projectId = undefined
  }
  crud.toQuery()
}
</script>
