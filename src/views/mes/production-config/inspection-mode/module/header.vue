<template>
  <component-radio-button
    v-model="query.productType"
    :options="typeEnum.ENUM"
    :unshowVal="[typeEnum.ENCLOSURE.V]"
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
  processMaterialListTypeEnum as typeEnum
  // processInspectTypeEnum as inspectTypeEnum,
  // processReportTypeEnum as reportTypeEnum
} from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
// import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  productType: typeEnum.ARTIFACT.V,
  inspectType: undefined,
  reportType: undefined,
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
