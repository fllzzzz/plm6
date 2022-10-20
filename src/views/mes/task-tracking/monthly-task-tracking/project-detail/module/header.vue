<template>
  <div class="head-container">
    <project-cascader v-model="query.projectId" placeholder="所属项目" clearable class="filter-item" style="width: 300px" />
    <monomer-select-area-select
      v-model:monomerId="query.monomerId"
      v-model:areaId="query.areaId"
      :productType="query.productType"
      needConvert
      clearable
      :project-id="query.projectId"
    />
    <el-input
      v-model.trim="query.serialNumber"
      size="small"
      placeholder="输入编号搜索"
      style="width: 170px"
      class="filter-item"
      clearable
      @keyup.enter="toQuery"
    />
    <rrOperation />
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import projectCascader from '@comp-base/project-cascader'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'

const defaultQuery = {
  projectId: undefined,
  monomerId: undefined,
  areaId: undefined,
  serialNumber: undefined
}

const { query } = regHeader(defaultQuery)
</script>

<style>
</style>
