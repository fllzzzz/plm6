<template>
  <crudOperation>
    <template #optLeft>
      <div v-show="crud.searchToggle">
        <project-radio-button size="small" v-model="query.projectId" class="filter-item" @change="crud.toQuery" />
        <el-date-picker
          v-model="query.dateTime"
          type="month"
          size="small"
          class="filter-item"
          placeholder="选择月"
          :clearable="false"
          value-format="x"
          @change="crud.toQuery"
        />
        <common-radio-button
          v-model="query.componentType"
          :options="reportComponentTypeEnum.ENUM"
          type="enum"
          class="filter-item"
          @change="handleComponentType"
        />
        <common-radio-button
          v-if="query.componentType === reportComponentTypeEnum.ARTIFACT.V"
          v-model="query.productType"
          :options="artifactProcessEnum.ENUM"
          type="enum"
          class="filter-item"
          @change="crud.toQuery"
        />
        <common-radio-button
          v-if="query.componentType === reportComponentTypeEnum.ENCLOSURE.V"
          v-model="query.category"
          :options="mesEnclosureTypeEnum.ENUM"
          type="enum"
          class="filter-item"
          @change="crud.toQuery"
        />
        <rrOperation />
      </div>
    </template>
  </crudOperation>
</template>

<script setup>
import { reportComponentTypeEnum, artifactProcessEnum, mesEnclosureTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  dateTime: new Date().getTime().toString(),
  productType: artifactProcessEnum.ONCE.V,
  componentType: reportComponentTypeEnum.ARTIFACT.V,
  category: mesEnclosureTypeEnum.PRESSED_PLATE.V
}

function handleComponentType(val) {
  if (val === reportComponentTypeEnum.ARTIFACT.V) {
    query.productType = artifactProcessEnum.ONCE.V
  } else {
    query.productType = reportComponentTypeEnum.ENCLOSURE.V
  }
  crud.toQuery()
}

const { crud, query } = regHeader(defaultQuery)
</script>
