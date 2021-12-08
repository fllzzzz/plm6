<template>
  <div class="head-container">
    <common-radio-button
      type="enum"
      v-model="query.basicClass"
      :options="rawMatClsEnum.ENUM"
      show-option-all
      clearable
      class="filter-item"
      @change="crud.toQuery"
    />
    <el-date-picker
      v-model="query.userUpdateTime"
      :default-time="defaultTime"
      type="daterange"
      range-separator=":"
      size="small"
      value-format="x"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      unlink-panels
      start-placeholder="申请开始日期"
      end-placeholder="申请结束日期"
      style="width: 240px"
      class="filter-item"
      @change="crud.toQuery"
    />
    <el-input
      v-model="query.operatorName"
      placeholder="可输入操作人名称搜索"
      class="filter-item"
      style="width: 200px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <rrOperation />
    <crudOperation />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  userUpdateTime: [], // [申请开始日期，申请结束日期]
  operatorName: undefined, // 操作人姓名 含领用人/申请人/审核人
  basicClass: undefined // 基础类型
}

const { crud, query } = regHeader(defaultQuery)
</script>
