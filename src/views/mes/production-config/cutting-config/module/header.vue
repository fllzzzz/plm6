<template>
  <div class="head-container">
    <common-select
      v-model="query.type"
      :options="cuttingConfigEnum.ENUM"
      type="enum"
      clearable
      placeholder="切割形式"
      class="filter-item"
      style="width: 200px"
      @change="crud.toQuery"
    />
    <common-select
      v-model="query.boolDrillEnum"
      :options="whetherEnum.ENUM"
      type="enum"
      clearable
      size="small"
      placeholder="是否开孔"
      class="filter-item"
      @change="crud.toQuery"
    />
    <el-input
      v-model="query.thickness"
      placeholder="可输入厚度搜索"
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
import { cuttingConfigEnum } from '@enum-ms/mes'
import { whetherEnum } from '@enum-ms/common'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  thickness: undefined, // 厚度
  type: undefined, // 切割形式
  boolDrillEnum: undefined // 是否打孔
}

const { crud, query } = regHeader(defaultQuery)
</script>
