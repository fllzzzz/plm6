<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <common-radio-button
        v-model="query.materialType"
        :options="materialPurchaseClsEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
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
        v-model="query.settlementStatus"
        :options="settlementStatusEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <!-- <common-radio-button
        v-model="query.orderSupplyType"
        :options="orderSupplyTypeEnum.ENUM"
        show-option-all
        type="enumSL"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      /> -->
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
      <!-- <warehouse-project-cascader
        v-model:projectId="query.projectId"
        v-model:projectWarehouseType="query.projectWarehouseType"
        class="filter-item"
        @change="crud.toQuery"
      /> -->
      <project-cascader
        v-model="query.projectId"
        placeholder="所属项目"
        clearable
        class="filter-item"
        style="width: 250px"
        @change="crud.toQuery"
      />
      <branch-company-select
        v-model="query.branchCompanyId"
        placeholder="合同签订主体"
        class="filter-item"
        style="width: 250px"
        @change="crud.toQuery"
      />
      <supplier-select
        v-model="query.supplierId"
        :type="query.purchaseType"
        clearable
        placeholder="可选择供应商搜索"
        show-hide
        class="filter-item"
        style="width: 250px"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.serialNumber"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按采购合同编号搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optLeft>
        <common-button
          v-permission="permission.add"
          class="filter-item"
          size="mini"
          type="primary"
          icon="el-icon-plus"
          @click.stop="crud.toAdd"
        >
          采购订单
        </common-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { inject, ref } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { purchaseStatusEnum } from '@enum-ms/wms'
import { materialPurchaseClsEnum } from '@enum-ms/classification'
import { settlementStatusEnum } from '@/utils/enum/modules/finance'

import { regHeader } from '@compos/use-crud'
import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'
import SupplierSelect from '@comp-base/supplier-select/index.vue'
import BranchCompanySelect from '@comp-base/branch-company-select.vue'
// import warehouseProjectCascader from '@comp-wms/warehouse-project-cascader'
const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  createTime: [], // [开始日期，结束日期]
  orderSupplyType: undefined, // 订单供货类型
  materialType: undefined, // 采购材料类型
  purchaseType: undefined, // 采购材料类型
  settlementStatus: undefined, // 结算状态
  purchaseStatus: purchaseStatusEnum.UNFINISHED.V, // 采购状态
  // projectWarehouseType: undefined, // 仓库类型
  projectId: undefined, // 项目id
  serialNumber: undefined, // 采购合同编号搜索
  branchCompanyId: undefined, // 签订主体
  supplierId: undefined, // 供应商id
  operatorName: undefined // 创建人 or 最后操作人
}

const permission = inject('permission')
const { crud, query } = regHeader(defaultQuery)

</script>
