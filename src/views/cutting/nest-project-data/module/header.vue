<template>
  <div>
    <div v-show="crud.searchToggle">
      <crudOperation>
        <template #optLeft>
          <el-input
            v-model="query.thick"
            placeholder="输入厚度搜索"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @blur="crud.toQuery"
          />
          <el-input
            v-model="query.plateType"
            placeholder="输入物料种类搜索"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @blur="crud.toQuery"
          />
          <el-date-picker v-model="date" type="date" size="small" class="filter-item" placeholder="请选择日期" @change="changeDate" />
          <rrOperation />
        </template>
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import rrOperation from '@crud/RR.operation'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import { ref } from 'vue'

const defaultQuery = {
  thick: undefined,
  plateType: undefined,
  importTime: undefined
}

const date = ref('')
const { crud, query, CRUD } = regHeader(defaultQuery)

function changeDate() {
  const y = new Date(date.value.getTime()).getFullYear()
  const m = new Date(date.value.getTime()).getMonth() + 1
  const d = new Date(date.value.getTime()).getDate()
  query.importTime = y + '-' + m + '-' + d
  crud.toQuery()
}

CRUD.HOOK.beforeResetQuery = () => {
  date.value = ''
}

</script>
