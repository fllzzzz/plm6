<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.enabled"
        :options="enabledEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <tableTypeCascader
        v-model="query.type"
        clearable
        filterable
        style="width: 300px;"
        class="filter-item"
        placeholder="可搜索表格"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.name"
        placeholder="输入表格名称搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #viewLeft>
        <el-tag size="medium">双击表格中的“行”，可更改表格默认模板</el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { enabledEnum } from '@enum-ms/common'
import tableTypeCascader from '@comp-common/print/table-type-cascader'

const defaultQuery = {
  name: undefined,
  enabled: enabledEnum.TRUE.V,
  type: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
