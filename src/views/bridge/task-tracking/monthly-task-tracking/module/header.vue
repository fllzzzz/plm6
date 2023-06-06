<template>
  <div class="head-container">
    <el-date-picker
      v-model="query.dateTime"
      type="year"
      size="small"
      class="date-item filter-item"
      style="width: 120px !important"
      format="YYYY"
      value-format="x"
      placeholder="选择年"
      :disabled-date="disabledDate"
      @change="crud.toQuery"
    />
    <workshop-select
      ref="workshopInfRef"
      v-model="query.workShopId"
      placeholder="请选择车间"
      :factory-id="query.factoryId"
      style="width: 200px"
      class="filter-item"
      clearable
      @change="crud.toQuery"
    />
    <production-line-select
      ref="productionLineRef"
      class="filter-item"
      v-model="query.productionLineId"
      :factory-id="query.factoryId"
      :workshop-id="query.workShopId"
      :productType="query.productType"
      placeholder="请选择生产线"
      style="width: 200px"
      clearable
      @change="crud.toQuery"
    />
    <rrOperation />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import rrOperation from '@crud/RR.operation'
import workshopSelect from '@/components-system/bridge/workshop-select'
import productionLineSelect from '@comp-bridge/production-line-select'

const defaultQuery = {
  dateTime: undefined,
  workShopId: undefined,
  productionLineId: undefined,
  productType: bridgeComponentTypeEnum.BOX.V
}

const { crud, query } = regHeader(defaultQuery)

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}
</script>

<style>
</style>
