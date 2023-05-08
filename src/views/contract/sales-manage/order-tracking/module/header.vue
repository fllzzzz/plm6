<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.orderSourceType"
        :options="orderSourceTypeEnum.ENUM"
        showOptionAll
        type="enumSL"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
       <common-radio-button
        v-model="query.projectStatus"
        :options="projectStatusEnum.ENUM"
        :unshowVal="[projectStatusEnum.SUSPEND.V]"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
       <!-- <common-radio-button
        v-model="query.projectStatus"
        :options="projectStatusEnum.ENUM"
        :unshowVal="[projectStatusEnum.SUSPEND.V]"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="projectStatusChange"
      /> -->
      <!-- <project-visa-select
        v-model="query.projectId"
        class="filter-item"
        style="width: 300px"
        @change="crud.toQuery"
        :projectStatus="status"
        :saveSettlement="query.projectStatus===projectStatusEnum.SETTLED.V?true:false"
        placeholder="可选择项目搜索"
        clearable
      /> -->
      <el-input
        v-model="query.name"
        placeholder="项目名称"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
      />
      <common-radio-button
        v-model="query.productType"
        :options="[packTypeEnum.STRUCTURE,packTypeEnum.ENCLOSURE]"
        default
        type="enumSL"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
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
      <rrOperation/>
    </div>
    <crudOperation>
      <template #viewLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="saleOrderTracking"
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
import { orderSourceTypeEnum, projectStatusEnum } from '@enum-ms/contract'
import { packTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
// import projectVisaSelect from '@comp-base/project-visa-select'

const defaultQuery = {
  year: undefined,
  projectStatus: undefined,
  orderSourceType: undefined
  // projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)
</script>
