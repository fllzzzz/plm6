<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <common-radio-button
      v-model="query.status"
      :options="improveStatusEnum.ENUM"
      showOptionAll
      type="enum"
      class="filter-item"
      @change="crud.toQuery"
    />
    <common-select
      v-model="query.type"
      :options="dict.breach_type"
      type="dict"
      size="small"
      clearable
      placeholder="问题类型"
      class="filter-item"
      @change="crud.toQuery"
    />
    <el-input
      v-model="query.number"
      placeholder="输入编号搜索"
      class="filter-item"
      style="width: 200px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <el-input
      v-model="query.applicant"
      placeholder="输入申请人搜索"
      class="filter-item"
      style="width: 200px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <el-date-picker
      v-model="date"
      :default-time="['00:00:00', '23:59:59']"
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
    <rrOperation />
  </div>
  <crudOperation />
</template>

<script setup>
import { reactive } from 'vue'

import { improveStatusEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  createTime: void 0,
  startDate: void 0,
  endDate: void 0,
  applicant: void 0,
  number: void 0,
  type: void 0,
  status: { value: improveStatusEnum.WAIT_RECTIFIED.V, resetAble: false },
  projectId: { value: void 0, resetAble: false }
}

const dict = useDict(['enterprise_type'])
const { crud, query } = regHeader(defaultQuery)
const date = reactive([])
function handleDateChange() {
  if (date && date.length > 1) {
    query.startDate = date[0]
    query.endDate = date[1]
  } else {
    query.startDate = void 0
    query.endDate = void 0
  }
  crud.toQuery()
}
</script>
