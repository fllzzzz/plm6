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
    <common-select
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
    />
    <common-radio-button
      v-model="query.businessType"
      :options="businessTypeEnum.ENUM"
      class="filter-item"
      :showOptionAll="false"
      type="enum"
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.orderSourceType"
      :options="orderSourceTypeEnum.ENUM"
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
  </div>
  <crudOperation>
    <template #viewLeft>
      <print-table
        v-permission="permission.print"
        api-key="fortuneReportList"
        :params="{ ...query }"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </template>
  </crudOperation>
</template>
<script setup>
import { inject } from 'vue'
import { parseTime } from '@/utils/date'
import { regHeader } from '@compos/use-crud'
import { businessTypeEnum, orderSourceTypeEnum, projectStatusEnum } from '@enum-ms/contract'
import crudOperation from '@crud/CRUD.operation'
// import projectCascader from '@comp-base/project-cascader.vue'
// import ExportButton from '@comp-common/export-button/index.vue'

const defaultQuery = {
  year: parseTime(new Date(), '{y}'),
  projectId: undefined,
  businessType: businessTypeEnum.MACHINING.V,
  orderSourceType: undefined,
  settlementStatus: undefined
}
const { crud, query } = regHeader(defaultQuery)
const permission = inject('permission')
const projectTree = inject('projectTree')
</script>
