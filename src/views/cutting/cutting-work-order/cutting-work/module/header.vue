<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <el-date-picker
            v-model="query.importTime"
            type="year"
            size="small"
            class="date-item filter-item"
            style="width: 100px !important"
            format="YYYY"
            value-format="YYYY"
            placeholder="选择年"
            :disabled-date="disabledDate"
            @change="crud.toQuery"
          />
          <!-- <common-radio-button
          style="margin-right: 8px"
          class="filter-item"
          v-model="query.nestingState"
          :options="NestingEnum.ENUM"
          show-option-all
          type="enum"
          size="small"
          @change="crud.toQuery"
        /> -->
        <common-radio-button
          style="margin-right: 8px"
          class="filter-item"
          v-model="query.nestingState"
          :options="SchedulingEnum.ENUM"
          show-option-all
          type="enum"
          size="small"
          @change="crud.toQuery"
        />
          <el-input
            v-model="query.projectName"
            placeholder="请输入项目名称"
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
import { regHeader } from '@compos/use-crud'
import { SchedulingEnum } from '@enum-ms/cutting'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { parseTime } from '@/utils/date'

const defaultQuery = {
  projectName: undefined,
  importTime:parseTime(new Date(), '{y}')
}
// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

const { crud, query } = regHeader(defaultQuery)
</script>
