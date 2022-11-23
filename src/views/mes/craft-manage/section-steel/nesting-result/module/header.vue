<template>
  <div class="header-container">
    <el-date-picker
      style="margin-right: 5px; width: 120px"
      v-model="query.year"
      type="year"
      size="small"
      class="filter-item"
      format="YYYY"
      value-format="YYYY"
      placeholder="请选择年"
      :disabled-date="disabledDate"
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.nestingStatusEnum"
      :options="[projectNestingStatusEnum.PARTIAL_NESTING, projectNestingStatusEnum.END_NESTING]"
      showOptionAll
      :optionAllValue="undefined"
      type="enum"
      class="filter-item"
      @change="crud.toQuery"
    />
  </div>
</template>

<script setup>
import { parseTime } from '@/utils/date'
import { regHeader } from '@compos/use-crud'
import { projectNestingStatusEnum } from '@enum-ms/mes'

const defaultQuery = {
  year: parseTime(new Date(), '{y}'),
  nestingStatusEnum: undefined
}
// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

const { crud, query } = regHeader(defaultQuery)
</script>
