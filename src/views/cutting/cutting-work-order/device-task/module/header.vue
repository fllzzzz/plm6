<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <!-- <el-date-picker
            v-model="query.importTime"
            type="year"
            size="small"
            class="date-item filter-item"
            style="width: 100px !important"
            format="YYYY"
            value-format="YYYY"
            placeholder="选择年"
            @change="crud.toQuery"
          /> -->
          <!-- <common-radio-button
          style="margin-right: 8px"
          class="filter-item"
          v-model="query.nestingState"
          :options="NestingEnum.ENUM"
          show-option-all
          type="enum"
          size="small"
          @change="crud.toQuery"
        /> -->
        <el-date-picker
            v-model="query.createTime"
            :default-time="defaultTime"
            type="daterange"
            range-separator=":"
            size="small"
            value-format="x"
            :shortcuts="PICKER_OPTIONS_SHORTCUTS"
            unlink-panels
            start-placeholder="开始日期"
            end-placeholder="结束日期"
            style="width: 240px"
            class="filter-item"
            @change="crud.toQuery"
          />
          <!-- <common-select
            v-model="query.factory"
            :options="factoryValue"
            placeholder="请选择工厂"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          /> -->
           <factory-select  
           v-model="query.factoryId" 
           placeholder="请选择工厂" 
           class="filter-item" 
           style="width: 270px"
           :factory-id="query.factoryId"
           @change="crud.toQuery" 
           />
           <!-- <common-select
            v-model="query.workshopInf"
            :options="workInfoValue"
            placeholder="请选择车间"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          /> -->
          <workshop-select
          v-model="query.workshopInfId"
          placeholder="请先选择车间"
          style="width: 270px"
          class="filter-item"
          :factory-id="query.factoryId"
          @change="crud.toQuery"  
          />
             <el-input
            v-model="query.machineName"
            placeholder="请输入设备名称"
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
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
// import { NestingEnum } from '@enum-ms/cutting'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import factorySelect from '@comp-base/factory-select.vue'
import workshopSelect from '@comp-mes/workshop-select'

const defaultQuery = {
  machineName: undefined,
  factoryId:undefined,
  workshopInfId:undefined,
  createTime:undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
