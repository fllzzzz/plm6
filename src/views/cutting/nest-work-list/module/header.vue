<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <el-date-picker
          v-model="query.importTime"
          type="year"
          size="small"
          class="date-item filter-item"
          style="width: 100px !important"
          format="YYYY"
          value-format="YYYY"
          placeholder="选择年"
          @change="crud.toQuery"
        />
        <el-input
          v-model="query.projectName"
          placeholder="请输入项目名称"
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
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  projectName: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
