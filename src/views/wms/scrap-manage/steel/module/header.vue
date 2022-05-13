<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <mat-header-query
        :basic-class="query.basicClass"
        :query="query"
        :to-query="crud.toQuery"
        show-project-warehouse-type
        :show-material-is-whole="false"
      >
        <template #afterProjectWarehouseType>
          <common-radio-button
            v-model="query.basicClass"
            :options="steelClsEnum.ENUM"
            type="enum"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
        </template>
      </mat-header-query>
      <rrOperation />
    </div>
    <crudOperation>
      <!-- TODO:打印 -->
      <template #optLeft></template>
    </crudOperation>
  </div>
</template>

<script setup>
import { rawMatClsEnum, steelClsEnum } from '@/utils/enum/modules/classification'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'

const defaultQuery = {
  projectId: { value: undefined, resetAble: false }, // 原项目id
  basicClass: { value: rawMatClsEnum.STEEL_PLATE.V, resetAble: false }
}

const { crud, query } = regHeader(defaultQuery)
</script>
