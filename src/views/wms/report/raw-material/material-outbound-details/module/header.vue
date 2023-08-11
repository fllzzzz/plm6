<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <mat-header-query :basic-class="query.basicClass" :query="query" :to-query="crud.toQuery">
        <template #afterProjectWarehouseType>
          <common-radio-button
            v-model="query.purchaseType"
            :options="baseMaterialTypeEnum.ENUM"
            type="enum"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
          <common-radio-button
            v-if="query.purchaseType & baseMaterialTypeEnum.RAW_MATERIAL.V"
            v-model="query.basicClass"
            :options="rawMatClsEnum.ENUM"
            show-option-all
            :unshowVal="[rawMatClsEnum.GAS.V]"
            type="enum"
            size="small"
            class="filter-item"
            @change="handleBasicClassChange"
          />
          <common-radio-button
            v-if="query.purchaseType & baseMaterialTypeEnum.MANUFACTURED.V"
            v-model="query.basicClass"
            :options="manufClsEnum.ENUM"
            show-option-all
            type="enum"
            size="small"
            class="filter-item"
            @change="handleBasicClassChange"
          />
          <common-radio-button
            v-if="query.purchaseType & baseMaterialTypeEnum.RAW_MATERIAL.V"
            v-model="query.orderSupplyType"
            :options="orderSupplyTypeEnum.ENUM"
            show-option-all
            type="enumSL"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
          <common-radio-button
            v-model="query.outboundAddress"
            :options="outboundDestinationTypeEnum.ENUM"
            show-option-all
            type="enum"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
        </template>
        <template #beforeWarehouse>
          <workshop-select
            v-model="query.workshopId"
            :type="warehouseTypeEnum.WORKSHOP.V"
            placeholder="车间"
            class="filter-item"
            style="width: 200px"
            clearable
            @change="crud.toQuery"
          />
        </template>
        <template #afterWarehouse>
          <!-- <workshop-select
            v-model="query.workshopId"
            :workshop-id="query.workshopId"
            placeholder="车间"
            class="filter-item"
            style="width: 200px"
            clearable
            @change="crud.toQuery"
          /> -->
        </template>
        <template #secondLineFirstItem>
          <warehouse-project-cascader
            v-model:projectId="query.projectId"
            v-model:projectWarehouseType="query.projectWarehouseType"
            class="filter-item"
            @change="crud.toQuery"
          />
          <monomer-select-area-select
            v-model:monomerId="query.monomerId"
            v-model:areaId="query.areaId"
            clearable
            areaClearable
            :filterArea="false"
            :project-id="query.projectId"
            :monomerDisabled="!query.projectId"
            :areaDisabled="!query.projectId"
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
            start-placeholder="审核时间"
            end-placeholder="审核时间"
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
            style="width: 150px"
            size="small"
            placeholder="申请/审核/领用人"
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
        <export-button v-permission="permission.get" :params="query" :fn="exportDetailsExcel" response-header-result>
          下载出库明细（根据查询条件）
        </export-button>
      </template>
      <template #viewLeft v-if="query.basicClass & STEEL_ENUM">
        <span v-permission="permission.get">
          <el-tag effect="plain" class="filter-item" size="medium">
            <span>总量：</span>
            <span v-if="!summaryLoading">
              {{ convertUnits(summaryMete, 'g', STEEL_BASE_UNIT.weight.unit, STEEL_BASE_UNIT.weight.precision) }}
              {{ STEEL_BASE_UNIT.weight.unit }}
            </span>
            <i v-else class="el-icon-loading" />
          </el-tag>
        </span>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { exportDetailsExcel, getSummary } from '@/api/wms/report/raw-material/outbound'
import { ref, inject, watchEffect } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { rawMatClsEnum, manufClsEnum } from '@enum-ms/classification'
import { orderSupplyTypeEnum, baseMaterialTypeEnum, warehouseTypeEnum, outboundDestinationTypeEnum } from '@/utils/enum/modules/wms'
import { STEEL_ENUM, STEEL_BASE_UNIT } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'

import { regHeader } from '@compos/use-crud'
import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'
import ExportButton from '@comp-common/export-button/index.vue'
import warehouseProjectCascader from '@comp-wms/warehouse-project-cascader'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import workshopSelect from '@/components-system/wms/workshop-select.vue'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  outboundTime: [], // [开始时间，结束时间]
  purchaseType: { value: baseMaterialTypeEnum.RAW_MATERIAL.V, resetAble: false },
  basicClass: undefined, // 物料类型
  orderSupplyType: undefined, // 供货类型
  projectId: undefined, // 项目id
  monomerId: undefined, // 单体id
  areaId: undefined, // 区域id
  workshopId: undefined, // 车间id
  projectWarehouseType: undefined, // 仓库类型
  outboundSN: undefined, // 出库单号
  operatorName: undefined // 申请/审核/领用人
}

const permission = inject('permission')
const { crud, query, CRUD } = regHeader(defaultQuery)

const summaryLoading = ref(false)
const summaryMete = ref()

CRUD.HOOK.beforeToQuery = () => {
  fetchSummaryInfo()
}

async function fetchSummaryInfo() {
  if (!(query.basicClass & STEEL_ENUM)) {
    return
  }
  summaryLoading.value = true
  try {
    summaryMete.value = (await getSummary(crud.query)) || 0
  } catch (error) {
    console.log('获取汇总数据', error)
  } finally {
    summaryLoading.value = false
  }
}

watchEffect(() => {
  if (query.purchaseType & baseMaterialTypeEnum.MANUFACTURED.V) {
    query.orderSupplyType = undefined
    query.basicClass = undefined
  }
  if (query.purchaseType & baseMaterialTypeEnum.RAW_MATERIAL.V) {
    query.basicClass = undefined
  }
})

// 基础类型发生变化
async function handleBasicClassChange(val) {
  await crud.resetQuery()
  query.basicClass = val
  crud.data = []
  crud.setColumns()
}
</script>
