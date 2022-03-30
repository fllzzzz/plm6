<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
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
        </div>
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
  // monomerValue: undefined,
  // areaValue: undefined
}

const { crud, query } = regHeader(defaultQuery)

</script>
