<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.sequenceType"
      :options="typeEnum.ENUM"
      class="filter-item"
      type="enum"
      size="small"
      @change="productTypeChange"
    />
    <common-radio-button
      v-if="query.sequenceType === typeEnum.ARTIFACT.V"
      v-model="query.type"
      :options="processTypeEnum.ENUM"
      size="small"
      class="filter-item"
      type="enum"
      @change="crud.toQuery"
    />
    <common-select
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
    />
    <rrOperation />
  </div>
  <crudOperation />
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import {
  processTypeEnum,
  processMaterialListTypeEnum as typeEnum,
  processInspectTypeEnum as inspectTypeEnum,
  processReportTypeEnum as reportTypeEnum
} from '@enum-ms/mes'

const defaultQuery = {
  type: processTypeEnum.ONCE.V,
  sequenceType: typeEnum.ARTIFACT.V,
  inspectType: undefined,
  reportType: undefined,
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)

function productTypeChange() {
  if (query.sequenceType === typeEnum.ARTIFACT.V) {
    query.type = processTypeEnum.ONCE.V
  } else {
    delete query.type
  }
  crud.toQuery()
}
</script>
