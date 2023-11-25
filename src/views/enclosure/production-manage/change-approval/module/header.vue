<template>
  <div v-show="crud.searchToggle">
    <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
    <common-radio-button
      v-model="query.handleStatusEnum"
      :options="changeHandleStatusEnum.ENUM"
      type="enum"
      showOptionAll
      class="filter-item"
      @change="crud.toQuery"
    />
  </div>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <!-- <monomer-select-area-select
          v-model:monomerId="query.monomerId"
          v-model:areaId="query.areaId"
          clearable
          areaClearable
          :project-id="query.projectId"
          :monomerDisabled="!query.projectId"
          :areaDisabled="!query.projectId"
          @change="crud.toQuery"
        /> -->
        <common-select
          placeholder="请选择区域"
          class="filter-item"
          v-model="query.areaId"
          clearable
        />
        <el-input
          v-model.trim="query.serialNumber"
          placeholder="输入编号搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>
<script setup>
// import {} from 'vue'
import crudOperation from '@crud/CRUD.operation'
import { changeHandleStatusEnum } from '@enum-ms/mes'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
// import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'

const defaultQuery = {
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
