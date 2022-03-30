<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.type"
        :options="logisticsSearchTypeEnum.ENUM"
        class="filter-item"
        type="enum"
        @change="crud.toQuery"
      />
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
      <el-input
        v-model="query.name"
        placeholder="项目名称/采购编号/物流公司搜索"
        style="width:230px"
        class="filter-item"
      />
      <rrOperation/>
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import { logisticsSearchTypeEnum } from '@enum-ms/contract'

const defaultQuery = {
  type: logisticsSearchTypeEnum.PRODUCT.V,
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)

</script>
