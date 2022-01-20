<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.createTime"
        type="daterange"
        range-separator=":"
        size="small"
        class="date-item filter-item"
        value-format="x"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width:240px"
      />
      <common-radio-button
        v-model="query.type"
        :options="supplierPayTypeEnum.ENUM"
        class="filter-item"
        :showOptionAll="true"
        type="enum"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.type"
        :options="hasPayEnum.ENUM"
        class="filter-item"
        :showOptionAll="true"
        type="enum"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.type"
        :options="hasTaxEnum.ENUM"
        class="filter-item"
        :showOptionAll="true"
        type="enum"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.supplierName"
        placeholder="供应商"
        style="width:200px"
        class="filter-item"
      />
      <rrOperation/>
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import { supplierPayTypeEnum, hasTaxEnum, hasPayEnum } from '@enum-ms/contract'

const defaultQuery = {
  type: undefined,
  supplierName: undefined
}

const { crud, query } = regHeader(defaultQuery)

</script>
