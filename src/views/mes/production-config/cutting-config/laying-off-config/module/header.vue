<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <el-input
          v-model="query.layingOffWayName"
          placeholder="可输入下料方式搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  layingOffWayName: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
