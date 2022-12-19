<template>
  <crudOperation>
    <template #optLeft>
      <common-radio-button
        v-model="query.productType"
        :options="deepenTypeEnum.ENUM"
        :unshowVal="[deepenTypeEnum.ARTIFACT.V]"
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
    </template>
    <template #viewLeft>
      <el-tag class="filter-item" size="medium" effect="plain">文件导入仅支持{{uploadType}}格式、ZIP压缩包格式（内容为{{uploadType}}格式）</el-tag>
    </template>
  </crudOperation>
</template>

<script setup>
import { computed } from 'vue'
import { deepenTypeEnum, technicalDataTypeEnum } from '@enum-ms/plan'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const uploadType = computed(() => query.productType === deepenTypeEnum.ASSEMBLY.V ? 'NC1' : 'DXF')

const defaultQuery = {
  dataType: technicalDataTypeEnum.NC_DRAWING.V,
  productType: deepenTypeEnum.ASSEMBLY.V
}

const { crud, query } = regHeader(defaultQuery)
</script>
