<template>
  <crudOperation>
    <template #optLeft>
      <common-radio-button
        v-model="query.productType"
        :options="globalProject.projectType === projectTypeEnum.STEEL.V?deepenTypeEnum.ENUM:bridgeProcessTypeEnum.ENUM"
        :unshowVal="globalProject.projectType === projectTypeEnum.STEEL.V?[deepenTypeEnum.ARTIFACT.V]:[bridgeProcessTypeEnum.BOX.V,bridgeProcessTypeEnum.CELL.V]"
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
import { computed, defineProps } from 'vue'
import { projectTypeEnum } from '@enum-ms/contract'
import { deepenTypeEnum, technicalDataTypeEnum } from '@enum-ms/plan'
import { bridgeProcessTypeEnum } from '@enum-ms/bridge'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'

const uploadType = computed(() => query.productType === deepenTypeEnum.ASSEMBLY.V ? 'NC1' : 'DXF')

const props = defineProps({
  globalProject: {
    type: Object,
    default: () => {}
  }
})

const defaultQuery = {
  dataType: technicalDataTypeEnum.NC_DRAWING.V,
  productType: props.globalProject.projectType === projectTypeEnum.STEEL.V ? deepenTypeEnum.ASSEMBLY.V : bridgeProcessTypeEnum.MACHINE_PART.V
}

const { crud, query } = regHeader(defaultQuery)
</script>
