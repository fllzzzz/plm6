<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.materialType"
        :options="materialPurchaseClsEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.status"
        :options="ddReviewStatusEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.enabled"
        :options="enabledEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.times"
        type="daterange"
        range-separator=":"
        size="small"
        value-format="x"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        unlink-panels
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width: 240px"
        class="filter-item"
        :disabledDate="(date) => {return date.getTime() > new Date().getTime()}"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.serialNumber"
        placeholder="可输入申购编号搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { ddReviewStatusEnum } from '@enum-ms/dd'
import { materialPurchaseClsEnum } from '@enum-ms/classification'
import { enabledEnum } from '@enum-ms/common'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  times: [], // [开始日期，结束日期]
  status: undefined,
  serialNumber: undefined,
  materialType: undefined
}
const { crud, query } = regHeader(defaultQuery)
</script>
