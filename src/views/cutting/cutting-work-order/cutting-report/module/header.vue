<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <!-- <el-date-picker v-model="date" type="date" size="small" class="filter-item" placeholder="请选择日期" @change="changeDate" /> -->
          <el-date-picker
            v-model="query.createTime"
            :default-time="defaultTime"
            type="daterange"
            range-separator=":"
            size="small"
            value-format="x"
            :shortcuts="PICKER_OPTIONS_SHORTCUTS"
            unlink-panels
            start-placeholder="开始日期"
            end-placeholder="结束日期"
            style="width: 240px"
            class="filter-item"
            @change="crud.toQuery"
          />
          <el-input
            v-model="query.workshopInf"
            placeholder="请输入车间"
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
import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

const date = ref('')
const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  workshopInf: undefined,
  online: undefined
}

const { CRUD, query, crud } = regHeader(defaultQuery)

// function changeDate() {
//   const y = new Date(date.value.getTime()).getFullYear()
//   const m = new Date(date.value.getTime()).getMonth() + 1
//   const d = new Date(date.value.getTime()).getDate()
//   query.importTime = y + '-' + m + '-' + d
//   crud.toQuery()
// }

CRUD.HOOK.beforeResetQuery = () => {
  date.value = ''
}

</script>
