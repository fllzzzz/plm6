<template>
  <div class="header-container">
    <project-radio-button size="small" v-model="query.projectId" :type="'all'" class="filter-item" @change="crud.toQuery" />
    <common-radio-button
      v-model="query.auxiliaryTypeEnum"
      :options="[auxiliaryMaterialTypeEnum.PEG, auxiliaryMaterialTypeEnum.SLEEVE]"
      showOptionAll
      class="filter-item"
      type="enum"
      @change="crud.toQuery"
    />
    <el-date-picker
      v-model="query.date"
      type="daterange"
      range-separator=":"
      size="small"
      value-format="x"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      unlink-panels
      start-placeholder="开始日期"
      end-placeholder="结束日期"
      style="width: 240px; margin-right: 10px"
      class="filter-item date-item"
      @change="handleDateChange"
    />
    <crudOperation>
      <template #viewLeft>
        <print-table
          api-key="mesStudSleeveStatisticsList"
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
import { auxiliaryMaterialTypeEnum } from '@enum-ms/mes'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import moment from 'moment'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  projectId: undefined,
  auxiliaryTypeEnum: undefined,
  date: [moment().subtract(1, 'week').valueOf(), moment().valueOf()],
  startDate: moment().subtract(1, 'week').valueOf(),
  endDate: moment().valueOf()
}

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

<style>
</style>
