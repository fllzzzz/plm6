<template>
  <div>
    <el-input
      v-model="query.label"
      placeholder="输入字典标签查询"
      class="filter-item"
      style="width: 200px"
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
  label: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
