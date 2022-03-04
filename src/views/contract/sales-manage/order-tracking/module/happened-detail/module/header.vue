<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
        <monomer-select
          v-model="query.monomerId"
          :project-id="query.projectId"
          :default="false"
          clearable
          class="filter-item"
          @change="crud.toQuery"
        />
        <el-date-picker
          v-model="query.date"
          type="daterange"
          range-separator=":"
          size="small"
          class="filter-item date-item"
          start-placeholder="开始时间"
          end-placeholder="结束时间"
          style="width: 240px"
          @change="handleDateChange"
        />
        <el-input
          v-model="query.name"
          placeholder="可输入名称搜索"
          class="filter-item"
          style="width: 200px;"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model="query.serialNumber"
          placeholder="输入编号搜索"
          class="filter-item"
          style="width: 200px;"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation/>
      </div>
    <crudOperation>
      <template #optLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="projectHappenedDetail"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import moment from 'moment'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'

const defaultQuery = {
  date: undefined, auditStartDate: undefined, auditEndDate: undefined,
  name: undefined, serialNumber: undefined,
  projectId: { value: undefined, resetAble: false },
  monomerId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.auditStartDate = moment(query.date[0]).valueOf()
    query.auditEndDate = moment(query.date[1]).valueOf()
  } else {
    query.auditStartDate = undefined
    query.auditEndDate = undefined
  }
  crud.toQuery()
}
</script>
