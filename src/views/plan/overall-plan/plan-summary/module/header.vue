<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.year"
        type="year"
        placeholder="请选择"
        format="YYYY"
        value-format="YYYY"
        class="filter-item"
        @change="crud.toQuery"
        style="width:120px;"
      />
      <common-radio-button
        v-model="query.type"
        :options="TechnologyTypeAllEnum.ENUM"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
    </div>
    <crudOperation>
      <template #optLeft>
        <el-tag>{{ query.type===TechnologyTypeAllEnum.STRUCTURE.V ||  query.type===TechnologyTypeAllEnum.BRIDGE.V? '单位(t)': '单位(m)' }}</el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'

const defaultQuery = {
  type: TechnologyTypeAllEnum.STRUCTURE.V,
  year: String(new Date().getFullYear())
}

const { crud, query } = regHeader(defaultQuery)
</script>
