<template>
  <div v-show="crud.searchToggle">
    <el-input
      v-model="query.blurry"
      placeholder="输入名称或者描述搜索"
      class="filter-item"
      style="width: 200px;margin-right:5px;"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <rrOperation />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
const defaultQuery = {
  blurry: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
