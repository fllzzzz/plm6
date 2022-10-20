<template>
  <div style="display: flex; justify-content: space-between">
    <div v-show="crud.searchToggle">
      <el-input
        v-model.trim="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <div>
      <slot name="auditBox"></slot>
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {}

const { crud, query } = regHeader(defaultQuery)
</script>
