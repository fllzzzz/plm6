<template>
  <component-radio-button
    v-model="query.productType"
    :options="typeEnum.ENUM"
    class="filter-item"
    type="enum"
    size="small"
    @change="crud.toQuery"
  />
  <!-- <common-select
    type="enum"
    v-model="query.reportType"
    :options="reportTypeEnum.ENUM"
    clearable
    placeholder="可选择上报方式"
    style="width: 200px"
    class="filter-item"
    @change="crud.toQuery"
  />
  <common-select
    type="enum"
    v-model="query.inspectType"
    :options="inspectTypeEnum.ENUM"
    clearable
    placeholder="可选择检验方式"
    style="width: 200px"
    class="filter-item"
    @change="crud.toQuery"
  /> -->
  <!-- <rrOperation /> -->
</template>

<script setup>
import {
  bridgeProcessTypeEnum as typeEnum
  // processInspectTypeEnum as inspectTypeEnum,
  // processReportTypeEnum as reportTypeEnum
} from '@enum-ms/bridge'

import { regHeader } from '@compos/use-crud'
// import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  productType: typeEnum.BOX.V,
  inspectType: undefined,
  reportType: undefined,
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
