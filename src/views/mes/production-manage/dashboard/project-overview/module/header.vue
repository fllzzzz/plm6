<template>
  <div v-show="crud.searchToggle">
    <el-date-picker
      v-model="query.year"
      type="year"
      size="small"
      class="date-item filter-item"
      style="width: 100px !important"
      placeholder="选择年"
      format="YYYY"
      value-format="YYYY"
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.projectStatus"
      :options="[projectStatusEnum.PROCESS, projectStatusEnum.COMPLETE]"
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
  </div>
</template>
<script setup>
import { projectStatusEnum } from '@enum-ms/contract'
import { parseTime } from '@/utils/date'
import { regHeader } from '@compos/use-crud'
const defaultQuery = {
  year: parseTime(new Date(), '{y}'),
  projectStatus: projectStatusEnum.PROCESS.V
}
const { crud, query } = regHeader(defaultQuery)
</script>
