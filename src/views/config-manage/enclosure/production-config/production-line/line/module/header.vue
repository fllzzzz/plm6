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
    <!-- <workshop-select
      v-model="query.workshopId"
      :factory-id="query.factoryId"
      placeholder="选择车间搜索"
      class="filter-item"
      style="width: 200px"
      clearable
      @change="crud.toQuery"
    /> -->
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
</template>

<script setup>
import { enabledEnum } from '@enum-ms/common'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import factorySelect from '@comp-base/factory-select.vue'

const defaultQuery = {
  factoryId: undefined,
  name: undefined,
  boolEnabledEnum: enabledEnum.TRUE.V
}

const { crud, query } = regHeader(defaultQuery)
</script>
