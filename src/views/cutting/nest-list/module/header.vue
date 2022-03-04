<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <crudOperation>
        <template #optLeft>
          <el-input
            v-model="query.projectName"
            placeholder="请输入项目名称"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model="query.monomerValue"
            placeholder="请选择单体"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model="query.areaValue"
            placeholder="请选择单元"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <rrOperation />
        </template>
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  projectName: undefined,
  monomerValue: undefined,
  areaValue: undefined
}

const { crud, query } = regHeader(defaultQuery)

</script>
