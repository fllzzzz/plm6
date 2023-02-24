<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.year"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width: 120px !important"
        placeholder="选择年"
        :clearable="false"
        format="YYYY"
        value-format="YYYY"
        :disabled-date="disabledDate"
        @change="crud.toQuery"
      />
      <project-cascader v-model="query.projectId" clearable class="filter-item" style="width: 300px" @change="crud.toQuery" />
      <common-select
        v-model="query.testingFeeTypeId"
        :options="dict.testing_fee_type"
        type="other"
        :data-structure="{ key: 'id', label: 'label', value: 'id' }"
        class="filter-item"
        clearable
        style="width: 200px"
        placeholder="检测费用类别"
        @change="crud.toQuery"
      />
      <rrOperation />
      <crudOperation />
    </div>
  </div>
</template>
<script setup>
import { inject } from 'vue'
import { parseTime } from '@/utils/date'
import { regHeader } from '@compos/use-crud'
import projectCascader from '@comp-base/project-cascader.vue'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const dict = inject('dict')

const defaultQuery = {
  year: parseTime(new Date(), '{y}'),
  projectId: undefined,
  testingFeeTypeId: undefined
}

const { crud, query } = regHeader(defaultQuery)

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}
</script>
