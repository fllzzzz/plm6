<template>
  <crudOperation>
    <template #optLeft>
      <common-radio-button
        v-model="query.productType"
        :options="deepenTypeEnum.ENUM"
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
    </template>
    <template #viewLeft>
      <el-tag class="filter-item" size="medium" effect="plain">文件导入仅支持{{uploadType}}、ZIP压缩包格式（内容为{{uploadType}}）</el-tag>
    </template>
  </crudOperation>
</template>

<script setup>
import { ref } from 'vue'
import { deepenTypeEnum, technicalDataTypeEnum } from '@enum-ms/plan'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const uploadType = ref('PDF格式、DWG格式')

const defaultQuery = {
  dataType: technicalDataTypeEnum.DEEPEN.V,
  productType: deepenTypeEnum.ARTIFACT.V
}

const { crud, query } = regHeader(defaultQuery)
</script>
