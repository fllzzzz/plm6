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
    <project-radio-button size="small" :type="'all'" v-model="query.projectType" class="filter-item" @change="crud.toQuery" />
    <!-- <project-cascader
      v-model="query.projectId"
      clearable
      style="width: 300px"
      class="filter-item"
      @change="crud.toQuery"
    /> -->
    <project-cascader v-model="query.projectId" clearable style="width: 270px" class="filter-item" @change="crud.toQuery" />
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
    </template>
    <template #viewLeft>
      <print-table
        v-permission="crud.permission.print"
        api-key="fortuneReportList"
        :params="{ ...query }"
        size="mini"
        type="warning"
      />
    </template>
  </crudOperation>
</template>
<script setup>
import { inject } from 'vue'

import { businessTypeEnum, projectStatusEnum } from '@enum-ms/contract'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
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
  dateQueryFormat: { value: undefined, resetAble: false },
  startDate: { value: undefined, resetAble: false },
  endDate: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

const projectTree = inject('projectTree')
console.log(projectTree)
</script>
