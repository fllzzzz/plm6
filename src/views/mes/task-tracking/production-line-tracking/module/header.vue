<template>
  <div class="head-container">
      <workshop-select
        ref="workshopInfRef"
        v-model="query.workshopInfId"
        placeholder="请选择车间"
        :factory-id="query.factoryId"
        style="width: 270px"
        class="filter-item"
        defaultValue
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.productType"
        :options="[componentTypeEnum.ARTIFACT,componentTypeEnum.ASSEMBLE,componentTypeEnum.MACHINE_PART]"
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
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import moment from 'moment'
import { componentTypeEnum } from '@enum-ms/mes'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import workshopSelect from '@comp-mes/workshop-select'

const defaultQuery = {
  workshop: undefined,
  date: [moment().subtract(1, 'month').valueOf(), moment().valueOf()],
  startDate: moment().subtract(1, 'month').valueOf(),
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
