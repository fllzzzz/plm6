<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.online"
        :options="messageTypeEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.machineNumber"
        placeholder="请输入设备编号"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
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
    <crudOperation />
  </div>
</template>

<script setup>
// import { supplierIsHideEnum } from '@enum-ms/supplier'

import { messageTypeEnum } from '@enum-ms/cutting'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  machineNumber: undefined,
  machineName: undefined,
  online: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
