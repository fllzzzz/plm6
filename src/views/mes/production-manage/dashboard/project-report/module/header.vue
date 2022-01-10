<template>
  <div v-show="crud.searchToggle">
    <monomer-select-area-select
      ref="monomerRef"
      v-model:monomerId="query.monomerId"
      v-model:areaId="query.areaId"
      :productType="query.category"
      :project-id="globalProjectId"
      monomerDefault
      areaDefault
      @change="crud.toQuery()"
    >
      <template #middle>
        <common-radio-button
          class="filter-item"
          v-model="query.category"
          :options="monomerProductTypeEnum"
          type="enum"
          size="small"
          @change="crud.toQuery"
        />
      </template>
    </monomer-select-area-select>
    <br/>
    <el-date-picker
      v-model="query.date"
      type="daterange"
      range-separator=":"
      size="small"
      class="filter-item date-item"
      start-placeholder="开始日期"
      end-placeholder="结束日期"
      style="width: 300px"
      :clearable="false"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      value-format="x"
      @change="handleDateChange"
    />
    <product-type-query :productType="productType" :category="query.category" :toQuery="crud.toQuery" :query="query" />
    <rrOperation />
  </div>
  <crudOperation />
</template>

<script setup>
import moment from 'moment'
import { computed, ref } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import EO from '@enum'
import { projectComponentTypeEnum, componentTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import productTypeQuery from '@comp-mes/header-query/product-type-query'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  category: projectComponentTypeEnum.ARTIFACT.V,
  date: [moment().startOf('month').valueOf(), moment().valueOf()],
  startDate: moment().startOf('month').valueOf(),
  endDate: moment().valueOf()
}

const { crud, query } = regHeader(defaultQuery)
const globalProjectId = useGlobalProjectIdChangeToQuery(crud)

function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startDate = query.date[0]
    query.endDate = query.date[1]
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}

const productType = computed(() => {
  return query.category === projectComponentTypeEnum.ARTIFACT.V ? componentTypeEnum.ARTIFACT.V : componentTypeEnum.ENCLOSURE.V
})

const monomerRef = ref()
const monomerProductTypeEnum = computed(() => {
  const _productType = monomerRef.value?.getProductType() || 0
  return EO.getBits(projectComponentTypeEnum.ENUM, _productType)
})
</script>
