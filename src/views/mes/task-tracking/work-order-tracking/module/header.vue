<template>
  <div class="head-container">
    <el-date-picker
      v-model="query.dateTime"
      type="month"
      size="small"
      class="date-item filter-item"
      style="width: 120px !important"
      format="YYYY-MM"
      value-format="x"
      placeholder="选择月"
      :disabled-date="disabledDate"
      @change="crud.toQuery"
    />
    <workshop-select
      ref="workshopInfRef"
      v-model="query.workshopId"
      placeholder="请选择车间"
      :factory-id="query.factoryId"
      style="width: 200px"
      class="filter-item"
      clearable
      defaultValue
      @change="crud.toQuery"
    />
    <production-line-select
      ref="productionLineRef"
      class="filter-item"
      v-model="query.productionLineId"
      :factory-id="query.factoryId"
      :workshop-id="query.workshopId"
      placeholder="请选择生产线"
      style="width: 200px"
      clearable
      defaultValue
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.productType"
      :options="[componentTypeEnum.ARTIFACT, componentTypeEnum.MACHINE_PART]"
      class="filter-item"
      type="enum"
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.status"
      :options="taskTrackingSchedulingStatusEnum.ENUM"
      showOptionAll
      class="filter-item"
      type="enum"
      @change="crud.toQuery"
    />
    <rrOperation />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import { componentTypeEnum, taskTrackingSchedulingStatusEnum } from '@enum-ms/mes'
import workshopSelect from '@comp-mes/workshop-select'
import productionLineSelect from '@comp-mes/production-line-select'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  dateTime: undefined,
  workshopId: undefined,
  productionLineId: undefined,
  productType: componentTypeEnum.ARTIFACT.V,
  status: undefined
}

const { crud, query } = regHeader(defaultQuery)

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

</script>

<style>
</style>
