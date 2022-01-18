<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.productType"
      :options="componentTypeEnum.ENUM"
      type="enum"
      :unshowVal="[componentTypeEnum.ENCLOSURE.V, componentTypeEnum.AUXILIARY_MATERIAL.V]"
      default
      class="filter-item"
      @change="crud.toQuery"
    />
    <project-radio-button size="small" :type="'all'" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <monomer-select-area-select
      v-model:monomerId="query.monomerId"
      v-model:areaId="query.areaId"
      :productType="query.productType"
      needConvert
      clearable
      :project-id="query.projectId"
      :monomerDisabled="!query.projectId"
      :areaDisabled="!query.projectId"
      @change="crud.toQuery"
    />
  </div>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <common-radio-button
          v-model="query.handleStatus"
          :options="surplusHandleStatusEnum.ENUM"
          type="enum"
          showOptionAll
          class="filter-item"
          @change="crud.toQuery"
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
        <el-input
          v-model.trim="query.processName"
          placeholder="输入工序搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model.trim="query.leaderName"
          placeholder="输入班组搜索"
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
import { componentTypeEnum, surplusHandleStatusEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import FactorySelect from '@/components-system/base/factory-select.vue'
import productionLineSelect from '@comp-mes/production-line-select'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {}

const { crud, query } = regHeader(defaultQuery)
</script>
