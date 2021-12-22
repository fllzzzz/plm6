<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.name"
        placeholder="输入公司名称搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.socialCode"
        placeholder="输入社会统一信用代码搜索"
        class="filter-item"
        style="width: 220px"
        size="small"
        clearable
         @blur="crud.toQuery"
      />
      <common-select
        v-model="query.enabled"
        :options="systemEnabledEnum.ENUM"
        type="enum"
        clearable
        placeholder="选择状态搜索"
        style="width: 200px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <rrOperation />
      <crudOperation />
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { systemEnabledEnum } from '@enum-ms/system'

const defaultQuery = {
  name: undefined,
  socialCode: undefined,
  enabled: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
