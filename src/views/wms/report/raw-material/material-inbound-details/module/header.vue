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
          <common-radio-button
            v-model="query.orderSupplyType"
            :options="orderSupplyTypeEnum.ENUM"
            show-option-all
            type="enumSL"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
          <common-radio-button
            v-model="query.rejectStatus"
            :options="materialRejectStatusEnum.ENUM"
            show-option-all
            type="enum"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
          <supplier-select
            v-model="query.supplierId"
            :basicClass="query.basicClass"
            :type="supplierTypeEnum.RAW_MATERIAL.V"
            mode="cross"
            clearable
            class="filter-item"
            placeholder="可选择供应商搜索"
            show-hide
            style="width: 250px"
            @change="crud.toQuery"
          />
        </template>
        <template #secondLineFirstItem>
          <warehouse-project-cascader
            v-model:projectId="query.projectId"
            v-model:projectWarehouseType="query.projectWarehouseType"
            class="filter-item"
            @change="crud.toQuery"
          />
          <el-date-picker
            v-model="query.inboundTime"
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
            style="width: 160px"
            size="small"
            placeholder="采购订单号"
            class="filter-item"
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model.trim="query.inboundSN"
            clearable
            style="width: 160px"
            size="small"
            placeholder="入库单号"
            class="filter-item"
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model.trim="query.licensePlate"
            clearable
            style="width: 160px"
            size="small"
            placeholder="车牌号"
            class="filter-item"
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model.trim="query.shipmentNumber"
            clearable
            style="width: 160px"
            size="small"
            placeholder="物流单号"
            class="filter-item"
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model.trim="query.operatorName"
            clearable
            style="width: 160px"
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
      <!-- 打印 -->
      <template #optLeft>
        <export-button v-permission="permission.get" :params="query" :fn="exportDetailsExcel" response-header-result>导出入库明细（根据查询条件）</export-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { exportDetailsExcel } from '@/api/wms/report/raw-material/inbound'
import { ref, inject } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { supplierTypeEnum } from '@enum-ms/supplier'
import { rawMatClsEnum } from '@enum-ms/classification'
import { materialRejectStatusEnum, orderSupplyTypeEnum } from '@/utils/enum/modules/wms'

import { regHeader } from '@compos/use-crud'
import ExportButton from '@comp-common/export-button/index.vue'

import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'
import SupplierSelect from '@comp-base/supplier-select/index.vue'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'
import warehouseProjectCascader from '@comp-wms/warehouse-project-cascader'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  inboundTime: [], // [开始时间，结束时间]
  basicClass: undefined, // 物料类型
  orderSupplyType: undefined, // 供货类型
  rejectStatus: undefined, // 退货状态
  projectId: undefined, // 项目id
  projectWarehouseType: undefined, // 仓库类型
  shipmentNumber: undefined, // 物流单号
  licensePlate: undefined, // 车牌号
  purchaseSN: undefined, // 采购单号
  inboundSN: undefined, // 入库单号
  supplierId: undefined, // 供应商id
  operatorName: undefined // 申请人/编辑人/审核人
}

const permission = inject('permission')
const { crud, query } = regHeader(defaultQuery)

// 基础类型发生变化
async function handleBasicClassChange(val) {
  await crud.resetQuery()
  query.basicClass = val
  crud.data = []
  crud.setColumns()
}
</script>
