<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <common-radio-button
            v-model="query.businessType"
            :options="businessTypeEnum.ENUM"
            showOptionAll
            type="enum"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
          <el-date-picker
            v-model="query.date"
            type="monthrange"
            range-separator=":"
            size="small"
            class="filter-item date-item"
            start-placeholder="开始月份"
            end-placeholder="结束月份"
            style="width: 200px"
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
          <rrOperation/>
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import moment from 'moment'
import { businessTypeEnum } from '@enum-ms/contract'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  name: undefined, startDate: undefined, endDate: undefined,
  businessType: { value: undefined, resetAble: false }
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
