<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.status"
        :options="reviewStatusEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <project-visa-select
        v-model="query.projectId"
        class="filter-item"
        style="width: 300px"
        @change="crud.toQuery"
        placeholder="可选择项目搜索"
        clearable
      />
      <el-date-picker
        v-model="query.year"
        placeholder="年份搜索"
        type="year"
        format="YYYY"
        value-format="YYYY"
        size="small"
        class="filter-item date-item"
        style="width: 120px"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.userName"
        placeholder="可输入申请人搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.checkUerName"
        placeholder="可输入审核人搜索"
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
import { reviewStatusEnum, visaTypeEnum } from '@enum-ms/common'
import { businessTypeEnum } from '@enum-ms/contract'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import projectVisaSelect from '@comp-base/project-visa-select'

const defaultQuery = {
  userName: undefined,
  checkUerName: undefined,
  year: undefined,
  type: visaTypeEnum.VISA.V,
  businessType: businessTypeEnum.MACHINING.V,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)
</script>
