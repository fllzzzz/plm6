<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <common-radio-button
      v-model="query.type"
      :options="problemTypeEnum.ENUM"
      showOptionAll
      type="enum"
      class="filter-item"
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.status"
      :options="improveStatusEnum.ENUM"
      showOptionAll
      type="enum"
      class="filter-item"
      @change="crud.toQuery"
    />
  </div>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <el-date-picker
          v-model="date"
          type="daterange"
          range-separator=":"
          size="small"
          class="filter-item date-item"
          value-format="x"
          start-placeholder="发起开始日期"
          end-placeholder="发起结束日期"
          style="width: 240px"
          @change="handleDateChange()"
        />
        <el-input
          v-model="query.userName"
          placeholder="输入发起人搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.dutyName"
          placeholder="输入整改人搜索"
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
</template>

<script setup>
import { ref } from 'vue'

import { problemTypeEnum } from '@enum-ms/production'
import { improveStatusEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  userName: void 0,
  dutyName: void 0,
  startDate: void 0,
  endDate: void 0,
  type: void 0,
  status: { value: improveStatusEnum.WAIT_RECTIFIED.V, resetAble: false },
  projectId: { value: void 0, resetAble: false }
}

const { crud, query } = regHeader(defaultQuery)
const date = ref([])
function handleDateChange() {
  if (date.value && date.value.length > 1) {
    query.startDate = date.value[0]
    query.endDate = date.value[1]
  } else {
    query.startDate = void 0
    query.endDate = void 0
  }
  crud.toQuery()
}
</script>
