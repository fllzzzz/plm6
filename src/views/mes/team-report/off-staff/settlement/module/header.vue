<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <common-radio-button
      v-model="query.componentType"
      :options="typeEnum.ENUM"
      type="enum"
      class="filter-item"
      @change="handleComponentType"
    />
    <common-radio-button
      v-if="query.componentType === typeEnum.ARTIFACT.V"
      v-model="query.productType"
      :options="artifactProcessEnum.ENUM"
      type="enum"
      class="filter-item"
      @change="crud.toQuery"
    />
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
    <rrOperation />
  </div>
  <crudOperation>
    <template #optLeft>
      <print-table
        v-permission="crud.permission.print"
        api-key="mesWageSummary"
        :params="{ ...query }"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </template>
  </crudOperation>
</template>

<script setup>
import moment from 'moment'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { processMaterialListTypeEnum as typeEnum, artifactProcessEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  date: [moment().subtract(1, 'month').valueOf(), moment().valueOf()],
  startDate: moment().subtract(1, 'month').valueOf(),
  endDate: moment().valueOf(),
  productType: artifactProcessEnum.ONCE.V,
  componentType: typeEnum.ARTIFACT.V
  // category: mesEnclosureTypeEnum.PRESSED_PLATE.V,
}

function handleComponentType(val) {
  if (val === typeEnum.ARTIFACT.V) {
    query.productType = artifactProcessEnum.ONCE.V
  } else {
    query.productType = val
  }
  crud.toQuery()
}

const { crud, query } = regHeader(defaultQuery)

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
