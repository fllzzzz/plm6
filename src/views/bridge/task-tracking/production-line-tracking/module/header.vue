<template>
  <div class="head-container">
    <common-radio-button
      v-model="query.productType"
      :options="[bridgeComponentTypeEnum .BOX, bridgeComponentTypeEnum .CELL, bridgeComponentTypeEnum .MACHINE_PART]"
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
      :clearable="false"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      unlink-panels
      start-placeholder="开始日期"
      end-placeholder="结束日期"
      style="width: 240px; margin-right: 10px"
      class="filter-item date-item"
      @change="handleDateChange"
    />
    <workshop-select
      ref="workshopInfRef"
      v-model="query.workShopId"
      placeholder="请选择车间"
      :factory-id="query.factoryId"
      style="width: 200px"
      class="filter-item"
      clearable
      defaultValue
      @change="crud.toQuery"
    />
    <rrOperation />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import moment from 'moment'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import workshopSelect from '@comp-mes/workshop-select'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  workShopId: undefined,
  productType: undefined,
  date: undefined,
  startTime: moment().subtract(1, 'month').valueOf(),
  endTime: moment().valueOf()
}

const { crud, query } = regHeader(defaultQuery)

function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startTime = query.date[0]
    query.endTime = query.date[1]
  } else {
    query.startTime = undefined
    query.endTime = undefined
  }
  crud.toQuery()
}
</script>

<style>
</style>
