<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.settlementStatus"
        :options="settlementStatusEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <project-cascader v-model="query.projectId" class="filter-item" />
      <el-date-picker
        v-model="query.createTime"
        type="daterange"
        range-separator=":"
        size="small"
        class="date-item filter-item"
        value-format="x"
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width:240px"
      />
      <el-input
        v-model.trim="query.supplierName"
        placeholder="供应商搜索"
        style="width:200px"
        class="filter-item"
      />
      <el-input
        v-model.trim="query.serialNumber"
        placeholder="订单号搜索"
        style="width:200px"
        class="filter-item"
      />
      <rrOperation/>
      <crudOperation />
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { baseMaterialTypeEnum } from '@enum-ms/wms'
import { settlementStatusEnum } from '@enum-ms/finance'
import projectCascader from '@comp-base/project-cascader'

const defaultQuery = {
  projectId: undefined,
  settlementStatus: settlementStatusEnum.UNSETTLEMENT.V,
  supplierName: undefined,
  serialNumber: undefined,
  purchaseType: { value: baseMaterialTypeEnum.MANUFACTURED.V, resetAble: false },
  createTime: [],
  startDate: undefined,
  endDate: undefined
}

const { crud, query } = regHeader(defaultQuery)

</script>
