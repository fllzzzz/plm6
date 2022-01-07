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
      <slot name="customSearch" :query="query"></slot>
      <monomer-select-area-select
        v-model:monomerId="query.monomerId"
        v-model:areaId="query.areaId"
        :productType="productType"
        needConvert
        clearable
        :project-id="query.projectId"
        @change="crud.toQuery"
      />
      <rrOperation />
    </template>
  </crudOperation>
</template>

<script setup>
import moment from 'moment'

import { regHeader } from '@compos/use-crud'
import FactorySelect from '@/components-system/base/factory-select.vue'
import productionLineSelect from '@comp-mes/production-line-select'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import { inject } from 'vue-demi'

const defaultQuery = {
  date: moment().valueOf() + ''
}

const productType = inject('productType')

const { crud, query } = regHeader(defaultQuery)
</script>
