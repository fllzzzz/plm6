<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <!-- <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" /> -->
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
          <!-- <el-date-picker
          v-model="query.localDateTime"
          type="month"
          range-separator=":"
          size="small"
          value-format="x"
          unlink-panels
          clearable
          placeholder="查询月份"
          style="width: 160px"
          class="filter-item"
          @change="crud.toQuery"
        /> -->
          <workshop-select
            v-model="query.workshopId"
            :workshop-type="workshopTypeEnum.BUILDING.V"
            placeholder="请选择车间"
            clearable
            style="width: 200px"
            class="filter-item"
            @change="crud.toQuery"
          />
          <el-input
            v-model="query.orderNumber"
            placeholder="输入任务单号搜索"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model.trim="query.serialNumber"
            placeholder="输入编号搜索"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
          <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
            重置
          </common-button>
          <!-- <rrOperation /> -->
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { boolPrintedEnum, workshopTypeEnum } from '@enum-ms/common'
import { componentTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
// import rrOperation from '@crud/RR.operation'
import workshopSelect from '@comp-mes/workshop-select'
// import moment from 'moment'

// const defaultTime = moment().startOf('month').valueOf()

const orderComponentTypeEnum = {
  ARTIFACT: componentTypeEnum.ARTIFACT,
  ASSEMBLE: componentTypeEnum.ASSEMBLE
}

const defaultQuery = {
  productType: componentTypeEnum.ARTIFACT.V,
  // localDateTime: defaultTime.toString()
  serialNumber: undefined
}

const { crud, query } = regHeader(defaultQuery)

function searchQuery() {
  crud.toQuery()
}
function resetQuery() {
  query.printType = undefined
  query.productType = componentTypeEnum.ARTIFACT.V
  query.workshopId = undefined
  query.orderNumber = undefined
  query.serialNumber = undefined
  crud.toQuery()
}
</script>
