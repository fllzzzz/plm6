<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <monomer-select
      v-model="query.monomerId"
      clearable
      :project-id="query.projectId"
      :default="false"
      class="filter-item"
      @change="crud.toQuery"
    />
    <factory-select v-model="query.factoryId" clearable class="filter-item" style="width: 200px" @change="crud.toQuery" />
    <el-date-picker
      v-model="query.date"
      type="daterange"
      range-separator=":"
      size="small"
      class="filter-item date-item"
      start-placeholder="开始日期"
      end-placeholder="结束日期"
      style="width: 240px"
      clearable
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      value-format="x"
      @change="handleDateChange"
    />
    <div>
      <slot name="customSearch" :query="query" />
      <product-type-query :productType="productType" :category="query.category" :toQuery="crud.toQuery" :query="query" />
      <rrOperation />
    </div>
  </div>
  <crudOperation>
    <template v-slot:viewLeft>
      <el-tag v-permission="permission.get" effect="plain" class="filter-item" size="medium">
        累计生产量：
        <slot v-if="!summaryLoading" name="summaryText" :summary="summaryData"></slot>
        <i v-else class="el-icon-loading" />
      </el-tag>
    </template>
  </crudOperation>
</template>

<script setup>
import { inject, ref } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import checkPermission from '@/utils/system/check-permission'
import { convertUnits } from '@/utils/convert/unit'
import { DP } from '@/settings/config'

import { regHeader } from '@compos/use-crud'
import monomerSelect from '@/components-system/plan/monomer-select'
import productTypeQuery from '@comp-mes/header-query/product-type-query'
import factorySelect from '@comp-base/factory-select'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = inject('defaultQuery')

const commonDefault = {
  date: [],
  startDate: undefined,
  endDate: undefined,
  factoryId: undefined,
  monomerId: undefined
}

const { crud, query, CRUD } = regHeader(Object.assign(commonDefault, defaultQuery))

const permission = inject('permission')
const productType = inject('productType')

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

CRUD.HOOK.afterToQuery = () => {
  fetchSummary()
}

const summaryData = ref({})
const summaryLoading = ref(false)
const getSummaryApi = inject('getSummaryApi')

async function fetchSummary() {
  if (!checkPermission(permission.get)) {
    return
  }
  try {
    summaryLoading.value = true
    const _data = await getSummaryApi(query)
    _data.totalArea = convertUnits(_data.totalArea, 'mm²', '㎡', DP.COM_AREA__M2)
    _data.totalLength = convertUnits(_data.totalLength, 'mm', 'm', DP.COM_L__M)
    summaryData.value = _data
  } catch (error) {
    console.log('获取汇总信息', error)
  } finally {
    summaryLoading.value = false
  }
}
</script>
