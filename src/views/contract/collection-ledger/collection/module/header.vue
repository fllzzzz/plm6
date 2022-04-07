<template>
  <div>
    <div v-show="crud.searchToggle">
      <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
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
        @change="crud.toQuery"
      />
      <common-select
        v-model="query.businessType"
        :options="businessTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="业务类型"
        style="width:200px"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.name"
        placeholder="项目名称"
        style="width:200px"
        class="filter-item"
      />
      <el-input
        v-model.trim="query.contractSignBodyName"
        placeholder="合同签订主体"
        style="width:200px"
        class="filter-item"
      />
      <rrOperation/>
    </div>
    <crudOperation>
      <template #optLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="collectionLedger"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { businessTypeEnum, auditTypeEnum } from '@enum-ms/contract'

const defaultQuery = {
  createTime: [],
  startDate: undefined,
  endDate: undefined,
  projectId: undefined,
  name: undefined,
  auditStatus: { value: auditTypeEnum.PASS.V, resetAble: false }
}

const { crud, query } = regHeader(defaultQuery)

</script>
