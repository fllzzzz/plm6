<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.boolEnabledEnum"
      :options="enabledEnum.ENUM"
      type="enum"
      showOptionAll
      class="filter-item"
      @change="crud.toQuery"
    />
    <factory-select
      v-model="query.factoryId"
      placeholder="选择工厂搜索"
      class="filter-item"
      style="width: 200px"
      clearable
      @change="crud.toQuery"
    />
    <workshop-select
      v-model="query.workshopId"
      :factory-id="query.factoryId"
      :workshop-type="workshopTypeEnum.BUILDING.V"
      placeholder="选择车间搜索"
      class="filter-item"
      style="width: 200px"
      clearable
      @change="crud.toQuery"
    />
    <common-select
      :dataStructure="{ key: 'K', label: 'L', value: 'V' }"
      v-model="query.productType"
      :options="componentTypeEnum.ENUM"
      :unshowOptions="[componentTypeEnum.AUXILIARY_MATERIAL.K]"
      placeholder="请选择产品类型"
      style="width: 190px"
      class="filter-item"
      clearable
      @change="crud.toQuery"
    />
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.productionLineTypeEnum"
        :options="hasIntelligent ? artifactProductLineEnum.ENUM : traditionLineEnum.ENUM"
        type="enum"
        showOptionAll
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.name"
        placeholder="输入生产线名称搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
      <crudOperation />
    </div>
  </div>
</template>

<script setup>
import { enabledEnum, workshopTypeEnum } from '@enum-ms/common'
import { componentTypeEnum, artifactProductLineEnum, traditionLineEnum } from '@enum-ms/mes'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import factorySelect from '@comp-base/factory-select.vue'
import workshopSelect from '@comp-mes/workshop-select'
import { mapGetters } from '@/store/lib'

const defaultQuery = {
  factoryId: undefined,
  name: undefined,
  boolEnabledEnum: enabledEnum.TRUE.V,
  productionLineTypeEnum: undefined,
  productType: undefined
}
const { hasIntelligent } = mapGetters('hasIntelligent')

const { crud, query } = regHeader(defaultQuery)
</script>
