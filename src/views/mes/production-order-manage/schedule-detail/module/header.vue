<template>
  <div v-show="crud.searchToggle" style="display: flex; justify-content: space-between">
    <el-date-picker
      v-model="query.localDateTime"
      type="year"
      size="small"
      value-format="x"
      :clearable="false"
      placeholder="选择年份"
      style="width: 120px"
      class="filter-item"
      @change="crud.toQuery"
    />
    <div style="width: 300px">
      <print-table v-permission="permission.print" :api-key="apiKey" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
    </div>
  </div>
</template>

<script setup>
import { inject } from 'vue'
import { regHeader } from '@compos/use-crud'

const permission = inject('permission')
const defaultQuery = {
  localDateTime: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
