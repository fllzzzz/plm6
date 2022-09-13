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
        v-model.trim="query.projectName"
        placeholder="项目"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.supplierName"
        placeholder="分包单位"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation />
    <!-- <common-drawer
      ref="drawerRef"
      :show-close="true"
      size="80%"
      title="提交记录"
      append-to-body
      v-model="applicationVisible"
      :close-on-click-modal="false"
    >
      <template #content>
        <paymentApplication :visibleValue="applicationVisible"/>
      </template>
    </common-drawer> -->
  </div>
</template>

<script setup>
import moment from 'moment'
// import { ref } from 'vue'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import { settlementStatusEnum } from '@enum-ms/finance'
// import paymentApplication from './payment-application'

const defaultQuery = {
  date: undefined,
  startDate: undefined,
  endDate: undefined,
  projectName: undefined,
  supplierName: undefined,
  boolSettlementStatus: settlementStatusEnum.UNSETTLEMENT.V
}
const { crud, query } = regHeader(defaultQuery)
// const applicationVisible = ref(false)
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
