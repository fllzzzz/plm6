<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
        <el-input
          v-model="query.name"
          placeholder="可输入名称搜索"
          class="filter-item"
          style="width: 200px;"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.serialNumber"
          placeholder="输入编号搜索"
          class="filter-item"
          style="width: 200px;"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation/>
      </div>
    <crudOperation>
      <template #optLeft>
        <!-- <print-table
          v-permission="crud.permission.print"
          api-key="projectHappenedDetail"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        /> -->
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  name: undefined, serialNumber: undefined
}
const { crud, query } = regHeader(defaultQuery)

</script>
