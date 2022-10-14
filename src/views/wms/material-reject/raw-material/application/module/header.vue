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
      <el-date-picker
        v-model="query.createTime"
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
        :type="supplierTypeEnum.RAW_MATERIAL.V"
        mode="cross"
        clearable
        class="filter-item"
        @change="crud.toQuery"
        placeholder="可选择供应商搜索"
        show-hide
        style="width: 250px"
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
        v-model.trim="query.serialNumber"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按入库单号搜索"
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
      <rrOperation />
    </div>
    <crudOperation>
      <!-- TODO:打印 -->
      <template #optLeft></template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import { useRoute } from 'vue-router'
import { PICKER_OPTIONS_SHORTCUTS, STEEL_ENUM } from '@/settings/config'
import { supplierTypeEnum } from '@enum-ms/supplier'
import { rawMatClsEnum } from '@enum-ms/classification'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import supplierSelect from '@comp-base/supplier-select/index.vue'
import warehouseProjectCascader from '@comp-wms/warehouse-project-cascader'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  createTime: [], // [开始日期，结束日期]
  basicClass: undefined, // 采购类型
  reviewStatus: undefined, // 审核状态
  projectId: undefined, // 项目id
  projectWarehouseType: undefined, // 仓库类型
  purchaseSN: undefined, // 采购合同编号
  serialNumber: undefined, // 入库单号
  supplierId: undefined, // 供应商id
  operatorName: undefined // 创建人
}

const route = useRoute()
const { crud, query } = regHeader(defaultQuery)
onMounted(() => {
  if (+route.params.basicClass === STEEL_ENUM) {
    query.basicClass = rawMatClsEnum.STEEL_PLATE.V
  } else {
    query.basicClass = route.params.basicClass
  }
})
</script>
