<template>
  <!-- <crudOperation>
    <template #optLeft> -->
  <div v-show="crud.searchToggle">
    <!-- <project-cascader
      v-model="query.projectId"
      placeholder="请选择项目"
      class="filter-item"
      style="width: 300px"
      clearable
      @change="crud.toQuery"
    /> -->
    <!-- <component-radio-button
      v-if="showComponent"
      v-model="query.productType"
      :options="componentTypeEnum.ENUM"
      type="enumSL"
      default
      :unshowVal="
        organizationType
          ? [componentTypeEnum.ENCLOSURE.V, componentTypeEnum.AUXILIARY_MATERIAL.V, ...unshowVal]
          : [componentTypeEnum.ENCLOSURE.V, componentTypeEnum.AUXILIARY_MATERIAL.V, ...unshowVal]
      "
      class="filter-item"
      @change="crud.toQuery"
    /> -->
    <monomer-select-area-select
      v-model:monomerId="query.monomerId"
      v-model:areaId="query.areaId"
      :productType="query.productType"
      needConvert
      clearable
      :project-id="query.projectId"
      :monomerDisabled="!query.projectId"
      :areaDisabled="!query.projectId"
      @change="crud.toQuery"
    />
    <div>
      <common-radio-button
        v-model="query.productType"
        :options="[componentTypeEnum.ARTIFACT, componentTypeEnum.ASSEMBLE]"
        type="enum"
        default
        @change="crud.toQuery"
        class="filter-item"
      />
      </div>
    <!-- <monomer-select ref="monomerRef" v-model="query.monomerId" clearable :default="false" :project-id="projectId" class="filter-item" /> -->

  </div>
  <!-- </template>
  </crudOperation> -->
</template>

<script setup>
import { inject, ref, watch, defineExpose } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
// import crudOperation from '@crud/CRUD.operation'
// import rrOperation from '@crud/RR.operation'
// import monomerSelect from '@/components-system/plan/monomer-select'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'

// import useUnshowProductTypeByMode from '@compos/use-unshow-productType-by-mode.js'

const projectId = inject('projectId')
// const organizationType = inject('organizationType')

const defaultQuery = {
  monomerId: undefined,
  productType: componentTypeEnum.ARTIFACT.V
}

const { crud, query, CRUD } = regHeader(defaultQuery)
// const { unshowVal, showComponent } = useUnshowProductTypeByMode({
//   resetQuery: function () {
//     query.productType = undefined
//   }
// })

const monomerRef = ref()

CRUD.HOOK.beforeToQuery = () => {
  crud.query.projectId = projectId
}

watch(
  () => query.monomerId,
  (val) => {
    if (val) {
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function getMonomer() {
  return monomerRef.value?.getOption(query.monomerId) || {}
}

defineExpose({
  getMonomer
})
</script>
