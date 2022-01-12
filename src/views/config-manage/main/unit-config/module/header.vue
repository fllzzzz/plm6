<template>
  <div class="head-container">
    <common-radio-button
      v-model="query.enabled"
      :options="enabledEnum.ENUM"
      show-option-all
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
    <common-select
      v-model="query.type"
      :options="unitTypeEnum.ENUM"
      type="enum"
      placeholder="类型"
      class="filter-item"
      style="width: 200px"
      @change="crud.toQuery"
    />
    <el-input
      v-model="query.name"
      placeholder="可输入单位名称搜索"
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
import { enabledEnum, unitTypeEnum } from '@enum-ms/common'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  name: undefined, // 单位名称
  type: undefined, // 单位类型
  enabled: undefined // 状态
}

const { crud, query } = regHeader(defaultQuery)
</script>
