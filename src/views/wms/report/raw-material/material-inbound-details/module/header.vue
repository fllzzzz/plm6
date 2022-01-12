<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <mat-header-query :basic-class="query.basicClass" :query="query" :to-query="crud.toQuery" :show-material-is-whole="false">
        <template #afterProjectWarehouseType>
          <common-radio-button
            v-model="query.basicClass"
            :options="rawMatClsEnum.ENUM"
            show-option-all
            type="enum"
            size="small"
            class="filter-item"
            @change="handleBasicClassChange"
          />
          <supplier-select
            v-model="query.supplierId"
            :basicClass="query.basicClass"
            :type="supplierTypeEnum.RAW_MATERIAL.V"
            mode="cross"
            clearable
            class="filter-item"
            @change="crud.toQuery"
            placeholder="可选择供应商搜索"
            show-hide
            style="width: 250px"
          />
        </template>
        <template #secondLineFirstItem>
          <el-date-picker
            v-model="query.createTime"
            :default-time="defaultTime"
            type="daterange"
            range-separator=":"
            size="small"
            value-format="x"
            :shortcuts="PICKER_OPTIONS_SHORTCUTS"
            unlink-panels
            start-placeholder="入库时间"
            end-placeholder="入库时间"
            style="width: 240px"
            class="filter-item"
            @change="crud.toQuery"
          />
          <el-input
            v-model.trim="query.purchaseSN"
            clearable
            style="width: 200px"
            size="small"
            placeholder="采购订单号"
            class="filter-item"
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model.trim="query.serialNumber"
            clearable
            style="width: 200px"
            size="small"
            placeholder="入库单号"
            class="filter-item"
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model.trim="query.licensePlate"
            clearable
            style="width: 200px"
            size="small"
            placeholder="车牌号"
            class="filter-item"
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model.trim="query.shipmentNumber"
            clearable
            style="width: 200px"
            size="small"
            placeholder="物流单号"
            class="filter-item"
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model.trim="query.operatorName"
            clearable
            style="width: 200px"
            size="small"
            placeholder="申请人/编辑人/审核人"
            class="filter-item"
            @keyup.enter="crud.toQuery"
          />
          <br />
        </template>
      </mat-header-query>

      <rrOperation />
    </div>
    <crudOperation>
      <!-- TODO:打印 -->
      <template #optLeft></template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { supplierTypeEnum } from '@enum-ms/supplier'
import { rawMatClsEnum } from '@enum-ms/classification'

import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'

import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'
import SupplierSelect from '@comp-base/supplier-select/index.vue'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  createTime: [], // [开始时间，结束时间]
  basicClass: undefined, // 采购类型
  projectId: { value: undefined, resetAble: false }, // 项目id
  shipmentNumber: undefined, // 物流单号
  licensePlate: undefined, // 车牌号
  purchaseSN: undefined, // 采购单号
  serialNumber: undefined, // 入库单号
  supplierId: undefined, // 供应商id
  operatorName: undefined // 创建人
}

const { crud, query } = regHeader(defaultQuery)
useGlobalProjectIdChangeToQuery(crud)

// 基础类型发生变化
async function handleBasicClassChange(val) {
  await crud.resetQuery()
  query.basicClass = val
  crud.data = []
  crud.setColumns()
}
</script>
