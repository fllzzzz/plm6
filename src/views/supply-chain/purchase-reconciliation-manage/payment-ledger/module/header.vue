<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="listType"
        :options="listTypeEnum"
        type="enum"
        size="small"
        class="filter-item"
        @change="handleListTypeChange"
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
        v-model="query.serialNumber"
        placeholder="采购订单"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.supplierName"
        placeholder="供应商"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optRight>
        <print-table
          v-permission="crud.permission.print"
          :api-key="isOrderType ? 'orderPaymentLedger' : 'supplierPaymentLedger'"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { get as orderList } from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { getBySupplier as summaryList } from '@/api/supply-chain/purchase-reconciliation-manage/payment-ledger'
import { ref, computed, defineExpose } from 'vue'
import moment from 'moment'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  date: undefined,
  startDate: undefined,
  endDate: undefined,
  serialNumber: undefined,
  supplierName: undefined
}
const { crud, query } = regHeader(defaultQuery)

// 列表类型
const listTypeEnum = {
  ORDER: { L: '采购订单', K: 'ORDER', V: 1 },
  SUPPLIER: { L: '供应商', K: 'SUPPLIER', V: 2 }
}

const listType = ref(listTypeEnum.ORDER.V)

// 是否为订单列表
const isOrderType = computed(() => {
  return listType.value === listTypeEnum.ORDER.V
})

// 列表类型切换
function handleListTypeChange(val) {
  crud.data = []
  crud.loading = true
  crud.crudApi.get = val === listTypeEnum.ORDER.V ? orderList : summaryList
  crud.toQuery()
}

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

defineExpose({
  isOrderType
})
</script>
