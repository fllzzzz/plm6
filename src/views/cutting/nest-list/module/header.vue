<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <crudOperation>
        <template #optLeft>
          <el-date-picker
            style="margin-right: 5px; width: 150px"
            v-model="date"
            type="date"
            size="small"
            class="filter-item"
            placeholder="请选择日期"
            @change="changeDate"
          />

          <rrOperation />
        </template>
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const date = ref('')

const defaultQuery = {
  importTime: undefined
}

function changeDate() {
  const a = new Date(date.value.getTime()).getFullYear()
  const b = new Date(date.value.getTime()).getMonth() + 1
  const c = new Date(date.value.getTime()).getDate()
  query.importTime = a + '-' + b + '-' + c
  crud.toQuery()
}

const { crud, query } = regHeader(defaultQuery)

</script>
