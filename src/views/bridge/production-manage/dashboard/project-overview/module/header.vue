<template>
  <div v-show="crud.searchToggle">
    <el-date-picker
      v-model="query.date"
      type="year"
      size="small"
      class="date-item filter-item"
      style="width: 100px !important"
      placeholder="选择年"
      format="YYYY"
      value-format="x"
      :disabled-date="disabledDate"
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.status"
      :options="[projectStatusEnum.PROCESS, projectStatusEnum.COMPLETE, projectStatusEnum.SUSPEND]"
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
  </div>
  <div>
    <el-input
      v-model.trim="query.name"
      size="small"
      placeholder="输入项目搜索"
      style="width: 240px"
      class="filter-item"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <rrOperation />
  </div>
</template>
<script setup>
import { projectStatusEnum } from '@enum-ms/contract'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  date: undefined,
  status: projectStatusEnum.PROCESS.V,
  name: undefined
}

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

const { crud, query } = regHeader(defaultQuery)
</script>
