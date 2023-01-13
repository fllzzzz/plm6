<template>
  <div v-show="crud.searchToggle">
    <el-date-picker
      v-model="query.year"
      type="year"
      size="small"
      class="date-item filter-item"
      style="width: 100px !important"
      placeholder="选择年"
      format="YYYY"
      value-format="YYYY"
      @change="crud.toQuery"
    />
    <project-radio-button size="small" :type="'all'" v-model="query.projectType" class="filter-item" @change="crud.toQuery" />
    <project-cascader v-model="query.projectId" clearable style="width: 300px" class="filter-item" @change="crud.toQuery" />
    <common-radio-button
      v-model="query.businessType"
      :options="businessTypeEnum.ENUM"
      class="filter-item"
      :showOptionAll="false"
      type="enum"
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.orderSourceType"
      :options="orderSourceTypeEnum.ENUM"
      class="filter-item"
      :showOptionAll="false"
      type="enum"
      @change="crud.toQuery"
    />
    <common-radio-button
      v-model="query.settlementStatus"
      :options="projectStatusEnum.ENUM"
      class="filter-item"
      showOptionAll
      :unshowVal="[projectStatusEnum.COMPLETE.V]"
      type="enum"
      @change="crud.toQuery"
    />
    <rrOperation />
  </div>
  <crudOperation>
    <template #viewLeft>
      <export-button class="filter-item" :fn="fn" :params="{ ...query }"> 导出清单 </export-button>
    </template>
  </crudOperation>
</template>
<script setup>
import { parseTime } from '@/utils/date'
import { regHeader } from '@compos/use-crud'
import { businessTypeEnum, orderSourceTypeEnum, projectStatusEnum } from '@enum-ms/contract'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import projectCascader from '@comp-base/project-cascader.vue'
import ExportButton from '@comp-common/export-button/index.vue'

const defaultQuery = {
  year: parseTime(new Date(), '{y}'),
  projectId: undefined,
  businessType: businessTypeEnum.MACHINING.V,
  orderSourceType: orderSourceTypeEnum.INSIDE.V,
  settlementStatus: undefined

}
const { crud, query } = regHeader(defaultQuery)
</script>
