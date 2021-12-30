<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" :type="'all'" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <el-date-picker
      v-model="query.date"
      type="month"
      size="small"
      class="filter-item"
      placeholder="选择月"
      style="width: 250px"
      @change="crud.toQuery"
      value-format="x"
    />
    <factory-select v-model="query.factoryId" clearable class="filter-item" style="width: 250px" @change="crud.toQuery" />
    <production-line-select
      v-model="query.productionLineId"
      :factoryId="query.factoryId"
      clearable
      class="filter-item"
      style="width: 250px"
      @change="crud.toQuery"
    />
  </div>
  <crudOperation>
    <template v-slot:optLeft>
      <div v-show="crud.searchToggle">
        <slot name="customSearch" :query="query"></slot>
        <monomer-select
          v-model="query.monomerId"
          :default="false"
          clearable
          :project-id="query.projectId"
          class="filter-item"
          style="width: 250px"
          @getAreaInfo="getAreaInfo"
          @change="crud.toQuery"
        />
        <common-select
          v-model="query.areaId"
          :options="areaList"
          size="small"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          clearable
          :noDataText="query.monomerId ? '暂无数据' : '未选择单体'"
          class="filter-item"
          placeholder="请选择区域"
          style="width: 250px"
          @change="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { ref } from 'vue'
import moment from 'moment'

import { regHeader } from '@compos/use-crud'
import FactorySelect from '@/components-system/base/factory-select.vue'
import productionLineSelect from '@comp-mes/production-line-select'
import monomerSelect from '@/components-system/plan/monomer-select'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  date: moment().valueOf() + ''
}

const { crud, query } = regHeader(defaultQuery)

const areaList = ref([])
function getAreaInfo(list) {
  areaList.value = list
}
</script>
