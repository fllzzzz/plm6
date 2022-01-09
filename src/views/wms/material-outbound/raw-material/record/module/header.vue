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
    <el-input
      v-model="query.serialNumber"
      placeholder="出库单号"
      class="filter-item"
      style="width: 200px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <el-input
      v-model="query.operatorName"
      placeholder="领用人/申请人/审核人"
      class="filter-item"
      style="width: 200px"
      size="small"
      clearable
      @keyup.enter="crud.toQuery"
    />
    <br />
    <el-date-picker
      v-model="query.createTime"
      :default-time="defaultTime"
      type="daterange"
      range-separator=":"
      size="small"
      value-format="x"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      unlink-panels
      start-placeholder="申请日期"
      end-placeholder="申请日期"
      style="width: 240px"
      class="filter-item"
      @change="crud.toQuery"
    />
    <el-date-picker
      v-model="query.outboundTime"
      :default-time="defaultTime"
      type="daterange"
      range-separator=":"
      size="small"
      value-format="x"
      :shortcuts="PICKER_OPTIONS_SHORTCUTS"
      unlink-panels
      start-placeholder="出库日期"
      end-placeholder="出库日期"
      style="width: 240px"
      class="filter-item"
      @change="crud.toQuery"
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
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  projectId: { value: undefined, resetAble: false }, // 项目id
  createTime: [], // 申请日期
  outboundTime: [], // 出库日期
  serialNumber: undefined, // 出库单号
  operatorName: undefined, // 操作人姓名 含领用人/申请人/审核人
  basicClass: undefined // 基础类型
}

const { crud, query } = regHeader(defaultQuery)
useGlobalProjectIdChangeToQuery(crud)
</script>
