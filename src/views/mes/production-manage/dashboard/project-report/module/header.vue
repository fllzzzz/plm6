<template>
  <div v-show="crud.searchToggle">
    <monomer-select-area-tabs :project-id="globalProjectId" @change="fetchMonomerAndArea" />
    <common-radio-button class="filter-item" v-model="query.category" :options="projectComponentTypeEnum.ENUM" type="enum" size="small" @change="crud.toQuery" />
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
    <div>
      <product-type-query
        :productType="productType"
        :category="query.category"
        :enclosureShowItem="Boolean(query.category)"
        :toQuery="crud.toQuery"
        :query="query"
      />
      <rrOperation />
    </div>
  </div>
  <crudOperation />
</template>

<script setup>
import moment from 'moment'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { projectComponentTypeEnum, componentTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import productTypeQuery from '@comp-mes/header-query/product-type-query'
import monomerSelectAreaTabs from '@comp-base/monomer-select-area-tabs'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import { computed } from 'vue-demi'

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

function fetchMonomerAndArea({ monomerId, areaId }) {
  query.monomerId = monomerId
  query.areaId = areaId
  crud.toQuery()
}
</script>