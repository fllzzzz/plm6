<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.basicClass"
      :options="rawMatClsEnum.ENUM"
      showOptionAll
      type="enum"
      size="small"
      class="filter-item"
    />
    <el-input v-model.trim="query.name" placeholder="输入科目名称搜索" class="filter-item" style="width: 250px" size="small" clearable />
    <el-input v-model.trim="query.code" placeholder="输入科目编号搜索" class="filter-item" style="width: 250px" size="small" clearable />
  </div>
  <crudOperation />
</template>

<script setup>
import { rawMatClsEnum } from '@enum-ms/classification'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  basicClass: undefined,
  name: undefined,
  code: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
