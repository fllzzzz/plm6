<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.boolHide"
        :options="supplierIsHideEnum.ENUM"
        type="enum"
        showOptionAll
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.name"
        placeholder="请输入供应商名称"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { supplierIsHideEnum } from '@enum-ms/supplier'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  name: undefined,
  boolHide: supplierIsHideEnum.FALSE.V
}

const { crud, query } = regHeader(defaultQuery)
</script>
