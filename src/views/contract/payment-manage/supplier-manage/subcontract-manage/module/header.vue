<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.boolSettlementStatus"
        :options="settlementStatusEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
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
        v-model="query.projectName"
        placeholder="项目"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.supplierName"
        placeholder="分包单位"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #viewLeft>
        <common-button size="small" class="filter-item" type="primary" @click="applicationVisible=true">提交记录</common-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import moment from 'moment'

import { regHeader } from '@compos/use-crud'
import { settlementStatusEnum } from '@enum-ms/finance'
import { supplierPayTypeEnum } from '@enum-ms/contract'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  date: undefined,
  startDate: undefined,
  endDate: undefined,
  projectName: undefined,
  supplierName: undefined,
  boolSettlementStatus: settlementStatusEnum.UNSETTLEMENT.V,
  propertyTypes: supplierPayTypeEnum.SUBCONTRACT.V
}
const { crud, query } = regHeader(defaultQuery)

// 时间变动
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
