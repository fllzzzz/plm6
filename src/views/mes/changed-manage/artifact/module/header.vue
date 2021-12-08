<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
        <el-date-picker v-model="date" type="date" size="small" class="filter-item" placeholder="请选择变更日期" @change="crud.toQuery" />
        <el-input
          v-model.trim="query.name"
          placeholder="输入构件编号搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { ref } from 'vue'
import moment from 'moment'

import { componentTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  name: undefined,
  projectId: undefined
}

const date = ref()

const { crud, query, CRUD } = regHeader(defaultQuery)

CRUD.HOOK.beforeToQuery = () => {
  if (date.value) {
    query.date = moment(date.value).valueOf()
  } else {
    query.date = undefined
  }
  query.productType = componentTypeEnum.ARTIFACT.V
}
</script>
