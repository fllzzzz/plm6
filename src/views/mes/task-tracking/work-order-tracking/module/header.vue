<template>
  <div class="head-container">
    <el-date-picker
      v-model="query.dateTime"
      type="year"
      size="small"
      class="date-item filter-item"
      style="width: 100px !important"
      format="YYYY"
      value-format="x"
      placeholder="选择年"
      :disabled-date="disabledDate"
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
    <common-radio-button
      type="enum"
      v-model="query.weightStatus"
      :options="[weightTypeEnum.NET, weightTypeEnum.GROSS]"
      class="filter-item"
      @change="crud.toQuery"
    />
    <project-cascader
      v-model="query.projectId"
      clearable
      class="filter-item"
      style="width: 270px"
      @change="crud.toQuery"
    />
    <div>
      <monomer-select-area-select
        v-model:monomerId="query.monomerId"
        v-model:areaId="query.areaId"
        needConvert
        clearable
        :project-id="query.projectId"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.orderNumber"
        placeholder="输入工单号搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import { componentTypeEnum, taskTrackingSchedulingStatusEnum } from '@enum-ms/mes'
import projectCascader from '@comp-base/project-cascader.vue'
import { weightTypeEnum } from '@enum-ms/common'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  dateTime: new Date().getTime(),
  monomerId: undefined,
  areaId: undefined,
  productType: componentTypeEnum.ARTIFACT.V,
  projectId: undefined,
  status: undefined,
  orderNumber: undefined,
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
