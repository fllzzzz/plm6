<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.date"
        type="daterange"
        range-separator=":"
        size="small"
        value-format="x"
        class="filter-item date-item"
        start-placeholder="开始时间"
        end-placeholder="结束时间"
        style="width: 240px"
        @change="handleDateChange"
      />
      <common-radio-button
        v-model="query.supplierClassification"
        :options="materialLedgerClsEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.settlementStatus"
        :options="settlementStatusEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <supplier-select
        v-model="query.supplierId"
        :type="query.supplierClassification?(query.supplierClassification===materialLedgerClsEnum.MANUFACTURED.V?supplierTypeEnum.MANUFACTURED.V:supplierTypeEnum.RAW_MATERIAL.V):undefined"
        clearable
        class="filter-item"
        placeholder="可选择供应商搜索"
        show-hide
        style="width: 240px"
        @change="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optRight>
        <print-table
          v-permission="crud.permission.print"
          :api-key="'scmSupplierPaymentLedger'"
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
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import { settlementStatusEnum } from '@enum-ms/finance'
import { supplierTypeEnum } from '@enum-ms/supplier'
import { materialLedgerClsEnum } from '@/utils/enum/modules/classification'

import supplierSelect from '@comp-base/supplier-select/index.vue'

const defaultQuery = {
  date: [new Date(new Date().getFullYear() + '/01/01').getTime(), new Date().getTime()],
  startDate: new Date(new Date().getFullYear() + '/01/01').getTime(),
  endDate: new Date().getTime(),
  supplierName: undefined,
  supplierClassification: undefined,
  settlementStatus: settlementStatusEnum.UNSETTLEMENT.V
}
const { crud, query } = regHeader(defaultQuery)

// 列表类型切换
// function handleListTypeChange(val) {
//   crud.data = []
//   crud.loading = true
//   crud.crudApi.get = val === listTypeEnum.ORDER.V ? orderList : summaryList
//   crud.toQuery()
// }

// 时间变动
function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startDate = query.date[0]
    query.endDate = query.date[1]
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}

</script>
