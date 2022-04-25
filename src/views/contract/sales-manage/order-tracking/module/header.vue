<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <common-radio-button
            v-model="query.productType"
            :options="packTypeEnum.ENUM"
            showOptionAll
            type="enumSL"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
          <project-visa-select
            v-model="query.projectId"
            class="filter-item"
            style="width: 300px"
            @change="crud.toQuery"
            placeholder="可选择项目搜索"
            clearable
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
          <rrOperation/>
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import moment from 'moment'
import { packTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import projectVisaSelect from '@comp-base/project-visa-select'

const defaultQuery = {
  date: undefined, startDate: undefined, endDate: undefined,
  productType: { value: undefined, resetAble: false },
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startDate = moment(query.date[0]).valueOf()
    query.endDate = moment(query.date[1]).valueOf()
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}
</script>
