<template>
  <div>
    <project-cascader v-model="query.projectId" class="filter-item" style="width: 300px" clearable @change="crud.toQuery" />
    <el-input
      v-model="query.name"
      placeholder="可输入物流公司搜索"
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
import projectCascader from '@comp-base/project-cascader.vue'

const defaultQuery = {
  projectId: undefined,
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
