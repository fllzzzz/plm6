<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <common-radio-button
      v-model="query.productType"
      :options="componentTypeEnum.ENUM"
      type="enumSL"
      :unshowVal="[componentTypeEnum.ENCLOSURE.V,componentTypeEnum.AUXILIARY_MATERIAL.V]"
      default
      class="filter-item"
      @change="crud.toQuery"
    />
    <el-date-picker
      v-model="query.date"
      type="daterange"
      range-separator=":"
      size="small"
      class="filter-item date-item"
      start-placeholder="开始日期"
      end-placeholder="结束日期"
      style="width: 240px"
      :clearable="false"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      value-format="x"
      @change="handleDateChange"
    />
    <!-- <common-radio-button
        v-if="query.componentType === typeEnum.ENCLOSURE.V"
        v-model="query.category"
        :options="mesEnclosureTypeEnum.ENUM"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      /> -->
    <factory-select v-model="query.factoryId" clearable class="filter-item" style="width: 200px" @change="crud.toQuery" />
    <rrOperation />
  </div>
  <crudOperation>
    <template #optLeft>
      <print-table
        v-permission="crud.permission.print"
        api-key="mesPiecework"
        :params="{ ...query }"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </template>
    <template #viewLeft>
      <common-button v-permission="crud.permission?.summaryDetail" size="mini" @click="summaryDetailVisible = true" type="success">汇总查看</common-button>
    </template>
  </crudOperation>
  <summaryDetail v-model:visible="summaryDetailVisible" />
</template>

<script setup>
import { ref } from 'vue'
import moment from 'moment'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { componentTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import factorySelect from '@comp-base/factory-select'
import summaryDetail from './summary-detail'
const defaultQuery = {
  date: [moment().subtract(1, 'month').valueOf(), moment().valueOf()],
  startDate: moment().subtract(1, 'month').valueOf(),
  endDate: moment().valueOf()
}

const summaryDetailVisible = ref(false)

const { crud, query } = regHeader(defaultQuery)

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
