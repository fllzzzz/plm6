<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
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
          <common-radio-button
            v-model="typeEnumV"
            :options="typeEnum.ENUM"
            size="small"
            default
            class="filter-item"
            type="enum"
            @change="typeEnumChange"
          />
          <rrOperation />
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, defineEmits } from 'vue'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { typeEnum } from '@enum-ms/cutting'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

const defaultQuery = {
  projectName: undefined,
  online: undefined
}

const { crud, query, CRUD } = regHeader(defaultQuery)

const typeEnumV = ref(0)
const date = ref('')
const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])
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
  typeEnumV.value = 0
  emit('change', typeEnumV.value)
}

function typeEnumChange() {
  emit('change', typeEnumV.value)
}
</script>
