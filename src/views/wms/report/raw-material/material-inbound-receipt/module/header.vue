<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <common-radio-button
        type="enum"
        v-model="query.basicClass"
        :options="rawMatClsEnum.ENUM"
        show-option-all
        clearable
        class="filter-item"
        @change="crud.toQuery"
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
        :options="receiptRejectStatusEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
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
        start-placeholder="入库日期"
        end-placeholder="入库日期"
        style="width: 240px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <supplier-select
        v-model="query.supplierId"
        :basicClass="query.basicClass"
        :type="(supplierTypeEnum.RAW_MATERIAL.V | supplierTypeEnum.OTHER.V)"
        mode="cross"
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
        placeholder="申请人/编辑人/审核人"
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
      <branch-company-select
        v-model="query.branchCompanyId"
        placeholder="合同签订主体"
        class="filter-item"
        clearable
        style="width: 250px"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.purchaseSN"
        clearable
        style="width: 200px"
        size="small"
        placeholder="采购合同编号"
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
      <br />
      <el-input
        v-model.trim="query.shipmentNumber"
        clearable
        style="width: 200px"
        size="small"
        placeholder="物流单号"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <!-- 打印 -->
      <template #optLeft>
        <print-table
          v-permission="permission.get"
          api-key="wmsRmInboundReceipt"
          :params="selectionIds"
          :disabled="selectionIds.length === 0"
          size="mini"
          type="warning"
          class="filter-item"
          @success="crud.selectAllChange"
          @downloadSuccess="printSuccess"
          @print-success="printSuccess"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { receiptPrintRecord } from '@/api/wms/report/raw-material/inbound'
import { inject, ref, computed, onMounted } from 'vue'
import { useRoute } from 'vue-router'
import { PICKER_OPTIONS_SHORTCUTS, STEEL_ENUM } from '@/settings/config'
import { supplierTypeEnum } from '@enum-ms/supplier'
import { rawMatClsEnum } from '@enum-ms/classification'
import { receiptRejectStatusEnum, orderSupplyTypeEnum } from '@/utils/enum/modules/wms'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import supplierSelect from '@comp-base/supplier-select/index.vue'
import BranchCompanySelect from '@comp-base/branch-company-select.vue'
import warehouseProjectCascader from '@comp-wms/warehouse-project-cascader'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  inboundTime: [], // [开始日期，结束日期]
  basicClass: undefined, // 物料类型
  orderSupplyType: undefined, // 供货类型
  rejectStatus: undefined, // 退货状态
  projectId: undefined, // 项目id
  projectWarehouseType: undefined, // 仓库类型
  shipmentNumber: undefined, // 物流单号
  licensePlate: undefined, // 车牌号
  purchaseSN: undefined, // 采购合同编号
  serialNumber: undefined, // 入库单号
  supplierId: undefined, // 供应商id
  operatorName: undefined // 申请人/编辑人/审核人
}

const permission = inject('permission')
const route = useRoute()
const { crud, query } = regHeader(defaultQuery)

const selectionIds = computed(() => {
  return crud.selections.map((row) => row.id)
})

onMounted(() => {
  if (+route.params.basicClass === STEEL_ENUM) {
    query.basicClass = rawMatClsEnum.STEEL_PLATE.V
  } else {
    query.basicClass = route.params.basicClass
  }
})

async function printSuccess(params) {
  await receiptPrintRecord(params)
  crud.toQuery()
}
</script>
