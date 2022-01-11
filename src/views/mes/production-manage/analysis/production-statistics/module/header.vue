<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
        <common-radio-button
          v-model="query.productType"
          :options="reportComponentTypeEnum.ENUM"
          type="enum"
          class="filter-item"
          @change="crud.toQuery"
        />
        <el-date-picker
          v-model="query.dateTime"
          type="month"
          size="small"
          class="filter-item"
          placeholder="选择月"
          value-format="x"
          @change="crud.toQuery"
        />
        <monomer-select v-model="query.monomerId" clearable :project-id="query.projectId" :disabled="!query.projectId" class="filter-item" @change="crud.toQuery" />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { reportComponentTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'

const defaultQuery = {
  dateTime: new Date().getTime().toString(),
  productType: reportComponentTypeEnum.ARTIFACT.V
}

const { crud, query } = regHeader(defaultQuery)
</script>
