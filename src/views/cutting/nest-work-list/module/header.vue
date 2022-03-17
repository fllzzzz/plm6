<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <el-date-picker
          v-model="date"
          type="year"
          size="small"
          class="date-item filter-item"
          style="width: 100px !important"
          placeholder="选择年"
          @change="dateChange"
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
import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  projectName: undefined,
  online: undefined,
  year: undefined

}

const { crud, query, CRUD } = regHeader(defaultQuery)
const date = ref('')

function dateChange() {
  const y = new Date(date.value.getTime()).getFullYear()
  // const m = new Date(date.value.getTime()).getMonth() + 1
  // const d = new Date(date.value.getTime()).getDate()
  query.importTime = y
  console.log(' query.importTime.', query.importTime)

  crud.toQuery()
}

CRUD.HOOK.beforeResetQuery = () => {
  date.value = ''
}
</script>
