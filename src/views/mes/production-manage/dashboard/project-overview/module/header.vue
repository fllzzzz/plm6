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
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.status"
      :options="[projectStatusEnum.PROCESS, projectStatusEnum.COMPLETE]"
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
import moment from 'moment'

const defaultTime = moment().startOf('year').valueOf()

const defaultQuery = {
  date: defaultTime.toString(),
  status: projectStatusEnum.PROCESS.V,
  name: undefined
}
const { crud, query } = regHeader(defaultQuery)
</script>
