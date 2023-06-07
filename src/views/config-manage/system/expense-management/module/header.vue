<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-select
        v-model="query.costAscriptionEnum"
        :options="costAscriptionEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="费用归属"
        style="width: 200px"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.name"
        placeholder="输入费用类型搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { costAscriptionEnum } from '@enum-ms/config'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  name: undefined,
  costAscriptionEnum: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
