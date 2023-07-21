<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
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
      <common-radio-button
        type="enum"
        v-model="query.logisticsTransportType"
        :options="logisticsTransportTypeEnum.ENUM"
        show-option-all
        clearable
        placeholder="可选择运输方式"
        class="filter-item"
        @change="handleLogisticsTransportChange"
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
        :type="supplierTypeEnum.LOGISTICS.V"
        clearable
        class="filter-item"
        @change="crud.toQuery"
        placeholder="可选择供应商搜索"
        show-hide
        style="width: 250px"
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
      <br />
      <warehouse-project-cascader
        v-model:projectId="query.projectId"
        v-model:projectWarehouseType="query.projectWarehouseType"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.purchaseSN"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按采购合同编号搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.inboundSN"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按入库单号搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <!-- <el-input
        v-model.trim="query.shipmentNumber"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按物流单号（系统）搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      /> -->
      <el-input
        v-model.trim="query.licensePlate"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按车牌号搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.serialNumber"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按物流单号搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
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
import { baseMaterialTypeEnum } from '@enum-ms/wms'
import { logisticsTransportTypeEnum } from '@enum-ms/logistics'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import supplierSelect from '@comp-base/supplier-select/index.vue'
import warehouseProjectCascader from '@comp-wms/warehouse-project-cascader'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  createTime: [], // [开始日期，结束日期]
  purchaseType: undefined, // 采购类型
  logisticsTransportType: logisticsTransportTypeEnum.FREIGHT.V, // 物流运输方式
  projectWarehouseType: undefined, // 仓库类型
  projectId: undefined, // 项目id
  shipmentNumber: undefined, // 物流单号
  licensePlate: undefined, // 车牌号
  purchaseSN: undefined, // 采购合同编号
  inboundSN: undefined, // 入库单号
  serialNumber: undefined, // 物流单号（系统）
  supplierId: undefined, // 供应商id
  operatorName: undefined // 创建人
}

const { crud, query } = regHeader(defaultQuery)

// 物流信息变化
function handleLogisticsTransportChange() {
  crud.data = []
  crud.setColumns()
  crud.toQuery()
}
</script>
