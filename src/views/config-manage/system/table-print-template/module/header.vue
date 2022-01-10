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
      <common-select
        v-model="query.moduleType"
        :options="moduleTypeEnum.ENUM"
        showOptionAll
        :data-structure="{ key: 'K', label: 'L', value: 'K' }"
        type="other"
        all-label-text="所有模块"
        placeholder="选择模块"
        class="filter-item"
        @change="crud.toQuery"
      />
      <table-type-select
        v-model="query.type"
        :module-type="query.moduleType"
        :show-all="true"
        type="enum"
        filterable
        placeholder="选择表格类型"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.name"
        placeholder="输入表格名称搜索"
        class="filter-item"
        style="width: 150px;"
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
import { moduleTypeEnum } from '@/utils/print/table-type'
import tableTypeSelect from '@comp-common/print/table-type-select-2'

const defaultQuery = {
  name: undefined,
  enabled: enabledEnum.TRUE.V,
  type: undefined,
  moduleType: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
