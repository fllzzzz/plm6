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
      :workshop-type="workshopTypeEnum.BUILDING.V"
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
    <div>
      <common-radio-button
        type="enum"
        v-model="query.weightStatus"
        :options="[weightTypeEnum.NET, weightTypeEnum.GROSS]"
        class="filter-item"
        @change="crud.toQuery"
      />
      <rrOperation />
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import { componentTypeEnum } from '@enum-ms/mes'
import { weightTypeEnum, workshopTypeEnum } from '@enum-ms/common'
import rrOperation from '@crud/RR.operation'
import workshopSelect from '@/components-system/base/workshop-select.vue'
import productionLineSelect from '@comp-mes/production-line-select'

const defaultQuery = {
  dateTime: new Date().getTime().toString(),
  workShopId: undefined,
  productionLineId: undefined,
  productType: componentTypeEnum.ARTIFACT.V,
  weightStatus: weightTypeEnum.NET.V
}

const { crud, query } = regHeader(defaultQuery)

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}
</script>

<style>
</style>
