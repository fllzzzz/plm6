<template>
  <crudOperation>
    <template #viewLeft>
      <el-tag class="filter-item" size="medium" effect="plain">文件导入仅支持{{uploadType}}格式、ZIP压缩包格式（内容为{{uploadType}}格式）</el-tag>
    </template>
  </crudOperation>
</template>

<script setup>
import { ref } from 'vue'
import { deepenTypeEnum, technicalDataTypeEnum } from '@enum-ms/plan'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const uploadType = ref('XML')

const defaultQuery = {
  dataType: technicalDataTypeEnum.XML_FILE.V,
  productType: deepenTypeEnum.ARTIFACT.V
}

regHeader(defaultQuery)
</script>
