<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <common-radio-button
          v-model="query.productType"
          :options="[componentTypeEnum.ARTIFACT, componentTypeEnum.ASSEMBLE, componentTypeEnum.MACHINE_PART]"
          showOptionAll
          class="filter-item"
          type="enum"
          @change="crud.toQuery"
        />
         <common-radio-button
          type="enum"
          v-model="query.weightStatus"
          :options="[weightTypeEnum.NET, weightTypeEnum.GROSS]"
          class="filter-item"
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
          :workshop-type="workshopTypeEnum.BUILDING.V"
          :factory-id="query.factoryId"
          style="width: 200px"
          class="filter-item"
          clearable
          defaultValue
          @change="crud.toQuery"
        />
        <rrOperation />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import moment from 'moment'
import { componentTypeEnum } from '@enum-ms/mes'
import { weightTypeEnum, workshopTypeEnum } from '@enum-ms/common'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import workshopSelect from '@comp-mes/workshop-select'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultQuery = {
  workShopId: undefined,
  productType: undefined,
  weightStatus: weightTypeEnum.NET.V,
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
