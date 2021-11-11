<template>
  <div>
    <process-select class="filter-item" :multiple="false" clearable v-model="query.processId" style="width: 200px" @change="crud.toQuery"/>
    <el-input
      v-model="query.leaderName"
      placeholder="输入组长名称搜索"
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
import processSelect from '@comp-mes/process-select'

const defaultQuery = {
  processId: undefined,
  leaderName: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
