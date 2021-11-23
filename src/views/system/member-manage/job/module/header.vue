<template>
  <div>
    <div v-show="crud.searchToggle">
        <el-input
        v-model="query.name"
        placeholder="按岗位名称搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <common-radio-button
        v-model="query.enabled"
        :options="systemEnabledEnum.ENUM"
        type="enum"
        class="filter-item"
        size="small"
        @change="crud.toQuery"
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
import { systemEnabledEnum } from '@enum-ms/system'

const defaultQuery = {
  name: undefined,
  enabled: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
