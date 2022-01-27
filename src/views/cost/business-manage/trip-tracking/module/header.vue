<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <project-radio-button
        size="small"
        v-model="query.projectId"
        class="filter-item"
        @change="crud.toQuery"
      />
      <monomer-select
        ref="monomerRef"
        v-model="query.monomerId"
        clearable
        :default="false"
        :project-id="query.projectId"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.deliveryDate"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item date-item"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width:240px"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        @change="handleDateChange"
      />
      <el-input
        v-model="query.serialNumber"
        placeholder="可输入车次搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.licensePlate"
        placeholder="可输入车牌搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation/>
    </div>
    <crudOperation>
    </crudOperation>
  </div>
</template>

<script setup>
import moment from 'moment'

import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'

const defaultQuery = {
  serialNumber: undefined, licensePlate: undefined,
  auditStartDate: undefined, auditEndDate: undefined,
  projectId: { value: undefined, resetAble: false },
  monomerId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

function handleDateChange() {
  if (query.deliveryDate && query.deliveryDate.length > 1) {
    query.auditStartDate = moment(query.deliveryDate[0]).valueOf()
    query.auditEndDate = moment(query.deliveryDate[1]).valueOf()
  } else {
    query.auditStartDate = undefined
    query.auditEndDate = undefined
  }
  crud.toQuery()
}
</script>
