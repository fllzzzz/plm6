<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.orderSourceType"
        :options="orderSourceTypeEnum.ENUM"
        showOptionAll
        type="enumSL"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
       <common-radio-button
        v-model="query.projectStatus"
        :options="projectStatusEnum.ENUM"
        :unshowVal="[projectStatusEnum.SUSPEND.V]"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="projectStatusChange"
      />
      <project-visa-select
        v-model="query.projectId"
        class="filter-item"
        style="width: 300px"
        @change="crud.toQuery"
        :projectStatus="status"
        :saveSettlement="query.projectStatus===projectStatusEnum.SETTLED.V?true:false"
        placeholder="可选择项目搜索"
        clearable
      />
      <el-input
        v-model.trim="query.projectContent"
        size="small"
        placeholder="输入项目内容"
        style="width: 200px;"
        class="filter-item"
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
    <crudOperation>
      <template #viewLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="saleOrderTracking"
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
import { ref } from 'vue'
import moment from 'moment'
import { orderSourceTypeEnum, projectStatusEnum } from '@enum-ms/contract'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import projectVisaSelect from '@comp-base/project-visa-select'

const defaultQuery = {
  date: undefined, startDate: undefined, endDate: undefined,
  orderSourceType: undefined,
  projectContent: undefined,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)
const status = ref()
function projectStatusChange(val) {
  if (val !== projectStatusEnum.SETTLED.V) {
    status.value = val
  }
  crud.toQuery()
}

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
