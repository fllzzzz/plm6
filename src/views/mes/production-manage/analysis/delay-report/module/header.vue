<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
        <el-date-picker
          v-model="query.dateTime"
          type="month"
          size="small"
          class="filter-item"
          placeholder="选择月"
          :clearable="false"
          value-format="x"
          @change="crud.toQuery"
        />
        <component-radio-button
          v-model="query.productType"
          :options="componentTypeEnum.ENUM"
          :unshowVal="[componentTypeEnum.AUXILIARY_MATERIAL.V]"
          default
          type="enumSL"
          class="filter-item"
          @change="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { componentTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  dateTime: new Date().getTime().toString()
}

const { crud, query } = regHeader(defaultQuery)
</script>
