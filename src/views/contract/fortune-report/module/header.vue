<template>
  <div v-show="crud.searchToggle">
    <el-date-picker
      v-model="query.year"
      type="year"
      size="small"
      class="date-item filter-item"
      style="width: 120px !important"
      placeholder="立项年份"
      format="YYYY"
      value-format="YYYY"
      @change="crud.toQuery"
    />
    <project-radio-button size="small" :type="'all'" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <!-- <project-cascader
      v-model="query.projectId"
      clearable
      style="width: 300px"
      class="filter-item"
      @change="crud.toQuery"
    /> -->
    <!-- <project-cascader v-model="query.projectId" clearable style="width: 270px" class="filter-item" @change="crud.toQuery" /> -->
    <!-- <common-select
      v-model="query.projectId"
      :options="projectTree"
      type="other"
      :data-structure="{ key: 'id', label: 'serialNumberName', value: 'id' }"
      class="filter-item"
      clearable
      filterable
      style="width: 300px"
      placeholder="选择项目"
      @change="crud.toQuery"
    /> -->
    <common-radio-button
      v-model="query.businessType"
      :options="businessTypeEnum.ENUM"
      class="filter-item"
      showOptionAll
      type="enum"
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.settlementStatus"
      :options="projectStatusEnum.ENUM"
      class="filter-item"
      showOptionAll
      type="enum"
      @change="crud.toQuery"
    />
    <el-input
      v-model.trim="query.customerUnit"
      clearable
      style="width: 160px"
      size="small"
      placeholder="业主名称搜索"
      class="filter-item"
      @keyup.enter="crud.toQuery"
    />
    <el-input
      v-model.trim="query.projectManager"
      clearable
      style="width: 160px"
      size="small"
      placeholder="项目经理搜索"
      class="filter-item"
      @keyup.enter="crud.toQuery"
    />
    <rrOperation />
  </div>
  <crudOperation>
    <template #optLeft>
      <time-range-select :query="query" clearable class="filter-item" style="width: 270px" @change="crud.toQuery" />
      <el-date-picker
        class="filter-item"
        type="daterange"
        v-model="date"
        :disabled-date="disabledDate"
        value-format="x"
        @change="handleDateChange"
        start-placeholder="开始时间"
        end-placeholder="结束时间"
        :shortcuts="query.dateQueryFormat === dateQueryTypeEnum.YEAR.V ? PICKER_OPTIONS_DATE : PICKER_OPTIONS_SHORTCUTS"
        :disabled="query.dateQueryFormat === dateQueryTypeEnum.DAY.V"
      ></el-date-picker>
    </template>
    <template #viewLeft>
      <print-table v-permission="crud.permission.print" api-key="fortuneReportList" :params="{ ...query }" size="mini" type="warning" />
    </template>
  </crudOperation>
</template>
<script setup>
<<<<<<< HEAD
import { inject } from 'vue'

import { businessTypeEnum, projectStatusEnum } from '@enum-ms/contract'

=======
import { ref } from 'vue'
import { businessTypeEnum, projectStatusEnum, dateQueryTypeEnum } from '@enum-ms/contract'
import { inject } from 'vue'
>>>>>>> feature/fortune
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { PICKER_OPTIONS_SHORTCUTS, PICKER_OPTIONS_DATE } from '@/settings/config'
// import projectCascader from '@comp-base/project-cascader.vue'
import timeRangeSelect from '@comp-common/time-range-select/index'
// import projectCascader from '@comp-base/project-cascader.vue'
// import ExportButton from '@comp-common/export-button/index.vue'

const defaultQuery = {
  projectId: undefined,
  businessType: undefined,
  settlementStatus: undefined,
  customerUnit: undefined,
  projectManager: undefined,
  secondStartDate: undefined,
  secondEndDate: undefined,
  dateQueryFormat: { value: undefined, resetAble: false },
  startDate: { value: undefined, resetAble: false },
  endDate: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

const projectTree = inject('projectTree')
const date = ref([])
console.log(projectTree)

// 监听第一次日期筛选
// watch(
//   [() => query.startDate, () => query.endDate],
//   () => {
//     date.value[0] = query.startDate
//     date.value[1] = query.endDate
//     console.log(date.value)
//   }
// )

// 根据第一次筛选日期判定警用日期
const disabledDate = (date) => {
  if (query.dateQueryFormat === dateQueryTypeEnum.YEAR.V) {
    return date.getTime() < query.startDate || date.getTime() > query.endDate
  } else if (query.dateQueryFormat === dateQueryTypeEnum.MONTH.V) {
    return date.getTime() < query.startDate || date.getTime() > query.endDate
  }
}

const handleDateChange = (v) => {
  if (v) {
    query.secondStartDate = v[0]
    query.secondEndDate = v[1]
  } else {
    query.secondStartDate = undefined
    query.secondEndDate = undefined
  }
  crud.toQuery()
}
</script>
