<template>
  <div v-show="crud.searchToggle">
    <el-date-picker
      v-model="query.localDateTime"
      type="month"
      size="small"
      class="date-item filter-item"
      style="width: 130px !important"
      placeholder="选择月"
      format="YYYY/MM"
      value-format="x"
      @change="crud.toQuery"
    />
    <workshop-select
      v-model="query.workshopId"
      placeholder="请选择车间"
      :factory-id="query.factoryId"
      style="width: 270px"
      class="filter-item"
      defaultValue
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.processType"
      :options="mesMachinePartOrderTypeEnum.ENUM"
      showOptionAll
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
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
        <el-input
          v-model="query.orderNumber"
          placeholder="指令号搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </template>
    </crudOperation>
  </div>
</template>
<script setup>
import { regHeader } from '@compos/use-crud'
import { mesMachinePartOrderTypeEnum } from '@enum-ms/mes'
import { boolPrintedEnum } from '@enum-ms/common'
import workshopSelect from '@comp-mes/workshop-select'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  localDateTime: undefined,
  factoryId: undefined,
  workshopId: undefined,
  processType: undefined,
  printType: undefined,
  orderNumber: undefined
}
const { crud, query } = regHeader(defaultQuery)
</script>
