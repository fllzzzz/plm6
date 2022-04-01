<template>
  <div class="head-container">
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
          <el-date-picker
            v-model="query.year"
            type="year"
            size="small"
            class="date-item filter-item"
            style="width:100px!important"
            placeholder="选择年"
            format="YYYY"
            value-format="YYYY"
            @change="crud.toQuery"
          />
          <el-input
            v-model="query.supplierName"
            placeholder="物流公司"
            class="filter-item"
            style="width: 200px;"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <rrOperation/>
        </div>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  year: String(new Date().getFullYear()),
  supplierName: undefined
}
const { crud, query } = regHeader(defaultQuery)

</script>
