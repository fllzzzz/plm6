<template>
  <div>
    <div v-show="crud.searchToggle">
        <el-input
        v-model="query.name"
        placeholder="按部门名称搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
