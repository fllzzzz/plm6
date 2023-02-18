<template>
  <div v-show="crud.searchToggle">
    <!-- <common-radio-button
      v-model="query.boolNestCutEnum"
      :options="layOffWayTypeEnum.ENUM"
      showOptionAll
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    /> -->
    <common-radio-button
      v-model="query.issueStatusEnum"
      :options="issueStatusEnum.ENUM"
      :unshowVal="[issueStatusEnum.NOT_NESTING.V]"
      showOptionAll
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
    <div>
      <el-input
        v-model="query.monomerId"
        placeholder="单体搜索"
        class="filter-item"
        style="width: 160px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.orderNumber"
        placeholder="任务单号搜索"
        class="filter-item"
        style="width: 160px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.thick"
        placeholder="零件厚度搜索"
        class="filter-item"
        style="width: 160px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.material"
        placeholder="零件材质搜索"
        class="filter-item"
        style="width: 160px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
  </div>
  <!-- <crudOperation>
    <template #optRight>
      <slot name="optRight" />
    </template>
  </crudOperation> -->
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
// import { layOffWayTypeEnum } from '@enum-ms/uploading-form'
import { machinePartSchedulingIssueStatusEnum as issueStatusEnum } from '@enum-ms/mes'
// import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {}

// const { crud } = regHeader(defaultQuery)
const { crud, query } = regHeader(defaultQuery)
</script>
