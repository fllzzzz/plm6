<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <common-radio-button
      v-model="query.printType"
      :options="boolPrintedEnum.ENUM"
      type="enum"
      show-option-all
      class="filter-item"
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.productType"
      :options="orderComponentTypeEnum"
      type="enum"
      class="filter-item"
      @change="crud.toQuery"
    />
    <!-- <project-cascader
      v-model="query.projectId"
      placeholder="所属项目"
      clearable
      class="filter-item"
      style="width: 300px"
      @change="crud.toQuery"
    /> -->
  </div>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <el-date-picker
          v-model="query.localDateTime"
          type="month"
          range-separator=":"
          size="small"
          value-format="x"
          unlink-panels
          clearable
          :clearable="false"
          placeholder="查询月份"
          style="width: 160px"
          class="filter-item"
          @change="crud.toQuery"
        />
        <workshop-select
          v-model="query.workshopId"
          placeholder="请选择车间"
          clearable
          style="width: 200px"
          class="filter-item"
          @change="crud.toQuery"
        />
        <el-input
          v-model="query.orderNumber"
          placeholder="输入任务单号搜索搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { boolPrintedEnum } from '@enum-ms/common'
import { componentTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import workshopSelect from '@comp-mes/workshop-select'
import moment from 'moment'

const defaultTime = moment().startOf('month').valueOf()

const orderComponentTypeEnum = {
  ARTIFACT: componentTypeEnum.ARTIFACT,
  ASSEMBLE: componentTypeEnum.ASSEMBLE
}

const defaultQuery = {
  productType: componentTypeEnum.ARTIFACT.V,
  localDateTime: defaultTime.toString()
}

const { crud, query } = regHeader(defaultQuery)
</script>
