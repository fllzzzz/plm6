<template>
  <div>
    <el-date-picker
      v-model="query.year"
      type="year"
      size="small"
      class="date-item filter-item"
      style="width: 100px !important"
      placeholder="选择年"
      format="YYYY"
      value-format="YYYY"
      :disabled-date="disabledDate"
      @change="crud.toQuery"
    />
    <el-input
      v-model="query.name"
      placeholder="项目模糊搜索"
      class="filter-item"
      style="width: 140px"
      size="small"
      clearable
      @blur="crud.toQuery"
    />
    <rrOperation />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  name: undefined,
  state: undefined,
  year: ''
}

const { crud, query } = regHeader(defaultQuery)

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}
</script>
