<template>
  <div>
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
        v-model="query.projectName"
        type="text"
        class="filter-item"
        placeholder="项目名称"
        style="width: 220px;"
      />
      <el-input
        v-model="query.signerName"
        type="text"
        class="filter-item"
        placeholder="销售负责人"
        style="width: 220px;"
      />
      <el-input
        v-model="query.createName"
        type="text"
        class="filter-item"
        placeholder="创建人"
        style="width: 220px;"
      />
      <rrOperation/>
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  monomerId: undefined,
  productType: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
