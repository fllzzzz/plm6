<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
        <common-radio-button
          v-model="query.productType"
          :options="componentTypeEnum.ENUM"
          type="enumSL"
          :unshowVal="[componentTypeEnum.ENCLOSURE.V, componentTypeEnum.AUXILIARY_MATERIAL.V]"
          default
          class="filter-item"
          @change="crud.toQuery"
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
  name: undefined,
  projectId: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
