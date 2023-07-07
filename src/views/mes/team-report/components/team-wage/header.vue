<template>
  <div>
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <!-- <common-radio-button
      v-model="query.productType"
      :options="componentTypeEnum.ENUM"
      type="enumSL"
      :unshowVal="[componentTypeEnum.ENCLOSURE.V, componentTypeEnum.AUXILIARY_MATERIAL.V]"
      showOptionAll
      :optionAllValue="initProductType"
      class="filter-item"
      @change="crud.toQuery"
    /> -->
    <el-date-picker
      v-model="query.date"
      type="daterange"
      range-separator=":"
      size="small"
      class="filter-item date-item"
      start-placeholder="开始日期"
      end-placeholder="结束日期"
      style="width: 240px"
      :clearable="false"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      value-format="x"
      @change="handleDateChange"
    />
    <!-- <common-radio-button
        v-if="query.componentType === typeEnum.ENCLOSURE.V"
        v-model="query.category"
        :options="mesEnclosureTypeEnum.ENUM"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      /> -->
    <factory-select v-model="query.factoryId" clearable class="filter-item" style="width: 200px" @change="crud.toQuery" />
  </div>
  <div>
    <el-row v-loading="summaryLoading" :gutter="20" style="margin-bottom:15px;">
      <el-col :span="8" class="card-panel-col">
        <panel
          :name="`制成品生产量（${showUnit}）`"
          num-color="#1890ff"
          :end-val="summaryInfo.mete || 0"
          :precision="2"
        />
      </el-col>
      <el-col :span="8" class="card-panel-col">
        <panel name="工资总额（元）" num-color="#1890ff" :end-val="summaryInfo.price || 0" :precision="decimalPrecision.mes" />
      </el-col>
      <el-col :span="8" class="card-panel-col">
        <panel :name="`平均人工费（元/${showUnit}）`" num-color="#1890ff" :end-val="summaryInfo.per || 0" :precision="decimalPrecision.mes" />
      </el-col>
    </el-row>
  </div>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <common-radio-button
          v-model="query.organizationType"
          :options="teamAttributeEnum.ENUM"
          type="enum"
          showOptionAll
          class="filter-item"
          @change="crud.toQuery"
        />
        <el-input
          v-model.trim="query.workshopName"
          clearable
          style="width: 150px"
          size="small"
          placeholder="车间搜索"
          class="filter-item"
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model.trim="query.productionLineName"
          clearable
          style="width: 150px"
          size="small"
          placeholder="生产线搜索"
          class="filter-item"
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model.trim="query.processName"
          clearable
          style="width: 150px"
          size="small"
          placeholder="工序搜索"
          class="filter-item"
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
    <template #viewLeft>
      <print-table
        v-permission="crud.permission.print"
        :api-key="apiKey"
        :params="{ ...query }"
        size="mini"
        type="warning"
        class="filter-item"
      />
      <!-- <span v-permission="crud.permission.get">
        <el-tag effect="plain" size="medium" class="filter-item">
          <span>制成品生产量（{{ showUnit }}）：</span>
          <span v-if="!summaryLoading">{{ summaryInfo.mete }}</span>
          <i v-else class="el-icon-loading" />
        </el-tag>
        <el-tag effect="plain" size="medium" class="filter-item">
          <span>工资总额（元）：</span>
          <span v-if="!summaryLoading" v-to-fixed="{ k: 'YUAN', val: summaryInfo.price }"></span>
          <i v-else class="el-icon-loading" />
        </el-tag>
        <el-tag effect="plain" size="medium" class="filter-item" type="success">
          <span>平均人工费（元/{{ showUnit }}）：</span>
          <span v-if="!summaryLoading" v-to-fixed="{ k: 'YUAN', val: summaryInfo.per }"></span>
          <i v-else class="el-icon-loading" />
        </el-tag>
      </span> -->
    </template>
  </crudOperation>
</template>

<script setup>
import { getSummary } from '@/api/mes/team-report/in-staff/piecework-system'
import moment from 'moment'
import { computed, inject, ref } from 'vue'

import { DP } from '@/settings/config'
import { deepClone } from '@data-type/index'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { componentTypeEnum, teamAttributeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import Panel from '@/components/Panel'
import factorySelect from '@comp-base/factory-select'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const initProductType = inject('initProductType')

const defaultQuery = {
  productType: initProductType,
  date: [moment().subtract(1, 'month').valueOf(), moment().valueOf()],
  startDate: moment().subtract(1, 'month').valueOf(),
  endDate: moment().valueOf()
}

const { crud, query, CRUD } = regHeader(defaultQuery)

const summaryProductType = computed(() => {
  return componentTypeEnum.ENCLOSURE.V & query.productType ? componentTypeEnum.ENCLOSURE.V : componentTypeEnum.ARTIFACT.V
})

const apiKey = computed(() => {
  if (summaryProductType.value === componentTypeEnum.ARTIFACT.V) {
    return 'mesStructureTeamWage'
  }
  if (summaryProductType.value === componentTypeEnum.ENCLOSURE.V) {
    return 'mesEnclosureTeamWage'
  }
  return undefined
})

const summaryLoading = ref(false)
const showUnit = ref('')
const summaryInfo = ref({ mate: 0, price: 0, per: 0, mete_dp: 0 })

async function fetchSummaryInfo() {
  summaryLoading.value = true
  try {
    const { mate = 0, price = 0 } = await getSummary(Object.assign(deepClone(query), { productType: summaryProductType.value }))
    const { c_unit, unit, dp } = useProductSummaryMeteUnit({ productType: summaryProductType.value })
    showUnit.value = c_unit
    const mete = useProductMeteConvert({
      productType: summaryProductType.value,
      weight: { num: mate, to: unit, dp },
      length: { num: mate, to: unit, dp }
    })
    const per = mete && price ? price / mete : 0
    summaryInfo.value = {
      mete,
      mete_dp: DP[dp],
      price,
      per
    }
  } catch (error) {
    console.log('获取汇总数据', error)
  } finally {
    summaryLoading.value = false
  }
}

CRUD.HOOK.beforeRefresh = () => {
  fetchSummaryInfo()
}

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
</script>
