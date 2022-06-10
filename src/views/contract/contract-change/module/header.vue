<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.year"
        type="year"
        size="small"
        class="date-item filter-item"
        style="width:100px!important"
        placeholder="选择年"
        format="YYYY"
        value-format="YYYY"
        @change="crud.toQuery"
      />
      <project-radio-button size="small" :type="'all'" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <el-input
        v-model.trim="query.name"
        placeholder="项目搜索"
        style="width:200px"
        class="filter-item"
      />
      <el-input
        v-model.trim="query.userName"
        placeholder="发起人搜索"
        style="width:200px"
        class="filter-item"
      />
      <el-input
        v-model.trim="query.auditorName"
        placeholder="审核人搜索"
        style="width:200px"
        class="filter-item"
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
      <common-select
        v-model="query.type"
        :options="contractChangeTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        placeholder="变更类型"
        class="filter-item"
        @change="crud.toQuery"
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
import { auditTypeEnum, contractChangeTypeEnum } from '@enum-ms/contract'

const defaultQuery = {
  projectId: undefined,
  year: undefined,
  name: undefined,
  userName: undefined,
  type: undefined,
  auditStatus: undefined,
  auditorName: undefined
  // auditStatus: undefined,
  // leader: undefined,
  // source: undefined,
  // type: undefined
}
const { crud, query } = regHeader(defaultQuery)
</script>
