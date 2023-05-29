<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <common-radio-button
            v-model="query.type"
            :options="contractSaleTypeEnum.ENUM"
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
          <project-visa-select
            v-model="query.projectId"
            class="filter-item"
            style="width: 300px"
            clearable
            placeholder="可选择项目搜索"
            @change="crud.toQuery"
          />
          <rrOperation />
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { contractSaleTypeEnum } from '@enum-ms/mes'
import { reviewStatusEnum } from '@enum-ms/common'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import projectVisaSelect from '@comp-base/project-visa-select'

const defaultQuery = {
  type: contractSaleTypeEnum.STRUCTURE.V, status: undefined,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)
</script>
