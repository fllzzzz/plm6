<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <crudOperation>
        <template #optLeft>
          <el-date-picker
            style="margin-right: 5px; width: 150px"
            v-model="query.importTime"
            type="date"
            size="small"
            class="filter-item"
            format="YYYY-MM-DD"
            value-format="YYYY-MM-DD"
            placeholder="请选择日期"
            @change="crud.toQuery"
          />
          <rrOperation />
        </template>
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
// import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  importTime: undefined
}

const { crud, query } = regHeader(defaultQuery)

</script>
