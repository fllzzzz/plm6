<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <common-radio-button
        v-model="query.purchaseStatus"
        :options="purchaseStatusEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        type="enum"
        v-model="query.purchaseType"
        :options="baseMaterialTypeEnum.ENUM"
        show-option-all
        clearable
        placeholder="可选择物料种类"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.createTime"
        :default-time="defaultTime"
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
        @change="crud.toQuery"
      />
      <supplier-select
        v-model="query.supplierId"
        :type="query.purchaseType"
        clearable
        class="filter-item"
        @change="crud.toQuery"
        placeholder="可选择供应商搜索"
        show-hide
        style="width: 250px"
      />
      <el-input
        v-model.trim="query.serialNumber"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按订单号搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.operatorName"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按操作人搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { purchaseStatusEnum, baseMaterialTypeEnum } from '@enum-ms/wms'

import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import supplierSelect from '@comp-base/supplier-select/index.vue'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  createTime: [], // [开始日期，结束日期]
  purchaseType: undefined, // 采购类型
  purchaseStatus: purchaseStatusEnum.UNFINISHED.V, // 采购状态
  projectId: { value: undefined, resetAble: false }, // 项目id
  serialNumber: undefined, // 采购单号搜索
  supplierId: undefined, // 供应商id
  operatorName: undefined // 创建人 or 最后操作人
}

const { crud, query } = regHeader(defaultQuery)
useGlobalProjectIdChangeToQuery(crud)
</script>
