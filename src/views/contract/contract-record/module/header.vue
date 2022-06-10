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
      <project-radio-button size="small" :type="'all'"  v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
      <el-input
        v-model.trim="query.leader"
        placeholder="项目搜索"
        style="width:200px"
        class="filter-item"
      />
      <rrOperation/>
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  projectId: undefined,
  // settlementStatus: settlementStatusEnum.UNSETTLEMENT.V,
  projectType: undefined,
  businessType: undefined,
  projectContentId: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
