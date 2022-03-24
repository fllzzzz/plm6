<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <common-radio-button
            v-model="listType"
            :options="listTypeEnum"
            type="enum"
            size="small"
            class="filter-item"
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
            placeholder="采购单号"
            class="filter-item"
            style="width: 200px;"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model="query.supplierName"
            placeholder="供应商"
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
import { ref, computed, defineExpose } from 'vue'
import moment from 'moment'
import { supplierPayMentTypeEnum } from '@enum-ms/contract'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  date: undefined, startDate: undefined, endDate: undefined,
  serialNumber: undefined, supplierName: undefined,
  propertyType: supplierPayMentTypeEnum.MATERIAL.V
}
const { crud, query } = regHeader(defaultQuery)

// 列表类型
const listTypeEnum = {
  ORDER: { L: '按订单查看', K: 'ORDER', V: 1 },
  SUMMARY: { L: '按汇总查看', K: 'SUMMARY', V: 2 }
}

const listType = ref(listTypeEnum.ORDER.V)

// 是否为订单列表
const isOrderType = computed(() => {
  return listType.value === listTypeEnum.ORDER.V
})

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
