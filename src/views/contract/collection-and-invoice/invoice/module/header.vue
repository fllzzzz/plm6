<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-select
        v-model="query.dateType"
        :options="contractDateTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="日期类型"
        style="width:110px"
        @change="crud.toQuery"
      />
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
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <el-radio-group v-model="query.settlementStatus" size="small" class="filter-item"  @change="crud.toQuery">
        <el-radio-button :label="undefined">全部</el-radio-button>
        <el-radio-button
          v-for="item in settlementStatusEnum.ENUM"
          :key="item.V"
          :label="item.V"
        >
          {{ item.L }}
        </el-radio-button>
      </el-radio-group>
      <common-select
        v-model="query.invoiceType"
        :options="invoiceTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="发票类型"
        style="width:200px"
        @change="crud.toQuery"
      />
      <common-select
        v-model="query.auditStatus"
        :options="auditTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="状态"
        style="width:200px"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.auditorName"
        placeholder="审核人"
        style="width:200px"
        class="filter-item"
      />
      <el-input
        v-model="query.writtenByName"
        placeholder="填报人"
        style="width:200px"
        class="filter-item"
      />
      <rrOperation/>
      <crudOperation add-text="开票填报">
      </crudOperation>
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { settlementStatusEnum, auditTypeEnum, contractDateTypeEnum, invoiceTypeEnum } from '@enum-ms/contract'
import { ElRadioGroup } from 'element-plus'

const defaultQuery = {
  projectId: undefined,
  dateType: contractDateTypeEnum.ENUM.UPDATE_DATE.V,
  createTime: [],
  startDate: undefined,
  endDate: undefined,
  settlementStatus: settlementStatusEnum.UNSETTLEMENT.V,
  invoiceType: undefined,
  writtenByName: undefined
}

const { crud, query } = regHeader(defaultQuery)

</script>
