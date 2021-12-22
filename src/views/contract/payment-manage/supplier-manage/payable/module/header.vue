<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.type"
        :options="supplierPayTypeEnum.ENUM"
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
import { supplierPayTypeEnum } from '@enum-ms/contract'

const defaultQuery = {
  type: undefined,
  supplierName: undefined
}

const { crud, query } = regHeader(defaultQuery)

</script>
