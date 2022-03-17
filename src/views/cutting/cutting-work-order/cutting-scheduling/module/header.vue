<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <!-- <el-date-picker v-model="date" type="date" size="small" class="filter-item" placeholder="请选择日期" @change="changeDate" />
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
          v-model="query.thick"
          placeholder="请输入厚度（mm）"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        /> -->

        <common-radio-button
          v-model="TypeEnumV"
          :options="TypeEnum.ENUM"
          size="small"
          default
          class="filter-item"
          type="enum"
          @change="TypeEnumChange"
        />
        <rrOperation />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { TypeEnum } from '@enum-ms/cutting'

const defaultQuery = {
  projectName: undefined,
  thick: undefined,
  online: undefined
}
const { CRUD } = regHeader(defaultQuery)
// const { crud, query, CRUD } = regHeader(defaultQuery)

const TypeEnumV = ref(0)
const date = ref('')
const emit = defineEmits(['change'])
// function changeDate() {
//   const y = new Date(date.value.getTime()).getFullYear()
//   const m = new Date(date.value.getTime()).getMonth() + 1
//   const d = new Date(date.value.getTime()).getDate()
//   query.importTime = y + '-' + m + '-' + d
//   crud.toQuery()
// }

CRUD.HOOK.beforeResetQuery = () => {
  date.value = ''
  TypeEnumV.value = 0
}

function TypeEnumChange() {
  emit('change', TypeEnumV.value)
}
</script>
