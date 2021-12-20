<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.name"
        placeholder="输入费用类型名称搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter.native="crud.toQuery"
      />
      <rrOperation />
      <crudOperation />
    </div>
  </div>
</template>

<script setup>
import { defineProps, ref, watch } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  name: undefined,
}

const { crud, query } = regHeader(defaultQuery)
</script>
