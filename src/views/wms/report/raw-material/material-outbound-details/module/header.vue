<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <mat-header-query :basic-class="query.basicClass" :query="query" :to-query="crud.toQuery">
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
        </template>
        <template #secondLineFirstItem>
          <warehouse-project-cascader
            v-model:projectId="query.projectId"
            v-model:projectWarehouseType="query.projectWarehouseType"
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
            start-placeholder="出库时间"
            end-placeholder="出库时间"
            style="width: 240px"
            class="filter-item"
            @change="crud.toQuery"
          />
          <el-input
            v-model.trim="query.outboundSN"
            clearable
            style="width: 160px"
            size="small"
            placeholder="出库单号"
            class="filter-item"
            @keyup.enter="crud.toQuery"
          />
          <el-input
            v-model.trim="query.operatorName"
            clearable
            style="width: 140px"
            size="small"
            placeholder="申请人/审核人"
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
import { rawMatClsEnum } from '@enum-ms/classification'
import { orderSupplyTypeEnum } from '@/utils/enum/modules/wms'

import { regHeader } from '@compos/use-crud'

import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'
import warehouseProjectCascader from '@comp-wms/warehouse-project-cascader'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  outboundTime: [], // [开始时间，结束时间]
  basicClass: undefined, // 物料类型
  orderSupplyType: undefined, // 供货类型
  projectId: undefined, // 项目id
  projectWarehouseType: undefined, // 仓库类型
  outboundSN: undefined, // 出库单号
  operatorName: undefined // 申请人/编辑人/审核人
}

const { crud, query } = regHeader(defaultQuery)

// 基础类型发生变化
async function handleBasicClassChange(val) {
  await crud.resetQuery()
  query.basicClass = val
  crud.data = []
  crud.setColumns()
}
</script>
