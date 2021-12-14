<template>
  <div v-show="crud.searchToggle">
    <el-input
      v-model="query.name"
      size="small"
      placeholder="输入名称搜索"
      style="width: 170px"
      class="filter-item"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <el-input
      v-model="query.serialNumber"
      size="small"
      placeholder="输入编号搜索"
      style="width: 170px"
      class="filter-item"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <rrOperation />
  </div>
  <crudOperation />
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {}

const { crud, query } = regHeader(defaultQuery)
</script>
