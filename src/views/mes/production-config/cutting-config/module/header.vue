<template>
  <div v-show="crud.searchToggle">
    <crudOperation>
      <template #optRight>
        <el-input
          v-model.trim="query.name"
          clearable
          size="small"
          placeholder="输入切割形式搜索"
          class="filter-item"
          style="width: 200px"
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </template>
    </crudOperation>
  </div>
</template>
<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
const defaultQuery = {}
const { crud, query } = regHeader(defaultQuery)
</script>
