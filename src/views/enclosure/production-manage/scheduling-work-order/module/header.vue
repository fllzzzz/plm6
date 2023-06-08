<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.orderNumber"
        placeholder="工单号搜索"
        class="filter-item"
        style="width: 180px"
        size="small"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.userName"
        placeholder="排产人搜索"
        class="filter-item"
        style="width: 180px"
        size="small"
        clearable
        @blur="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #viewLeft>
      <slot name="viewLeft" />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  orderNumber: undefined,
  userName: undefined,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)
</script>
