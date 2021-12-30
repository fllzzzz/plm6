<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.name"
        placeholder="输入项目名称"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @blur="crud.toQuery"
      />
      <rrOperation />
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
