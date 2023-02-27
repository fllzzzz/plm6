<template>
  <div v-show="crud.searchToggle">
    <!-- <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" /> -->
    <!-- <common-radio-button
      v-model="query.processType"
      :options="mesMachinePartOrderTypeEnum.ENUM"
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    /> -->
    <common-radio-button
      v-model="query.printType"
      :options="boolPrintedEnum.ENUM"
      showOptionAll
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
    <crudOperation>
      <template #optLeft>
        <!-- <el-date-picker
          v-model="query.localDateTime"
          type="month"
          size="small"
          class="date-item filter-item"
          style="width: 130px !important"
          placeholder="选择月"
          format="YYYY-MM"
          value-format="x"
          :disabled-date="disabledDate"
          @change="crud.toQuery"
        /> -->
        <workshop-select
          v-model="query.workshopId"
          placeholder="请选择车间"
          :factory-id="query.factoryId"
          style="width: 200px"
          class="filter-item"
          defaultValue
          @change="crud.toQuery"
        />
        <el-input
          v-model="query.cutNumber"
          placeholder="指令号搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <!-- <rrOperation /> -->
        <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
        <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
          重置
        </common-button>
      </template>
    </crudOperation>
  </div>
</template>
<script setup>
import { regHeader } from '@compos/use-crud'
// import { mesMachinePartOrderTypeEnum } from '@enum-ms/mes'
import { boolPrintedEnum } from '@enum-ms/common'
import workshopSelect from '@comp-mes/workshop-select'
import crudOperation from '@crud/CRUD.operation'
// import rrOperation from '@crud/RR.operation'
// import moment from 'moment'

// const defaultTime = moment().startOf('month').valueOf()

const defaultQuery = {
  // projectId: undefined,
  // localDateTime: defaultTime.toString(),
  factoryId: undefined,
  workshopId: undefined,
  // processType: mesMachinePartOrderTypeEnum.CUTTING_ORDER.V,
  printType: undefined,
  cutNumber: undefined
}

// 如果时间选取的时间年份比当前的时间大就被禁用
// function disabledDate(time) {
//   return time > new Date()
// }

const { crud, query } = regHeader(defaultQuery)

function searchQuery() {
  crud.toQuery()
}
function resetQuery() {
  query.printType = undefined
  query.factoryId = undefined
  query.workshopId = undefined
  query.cutNumber = undefined
  crud.toQuery()
}
</script>
