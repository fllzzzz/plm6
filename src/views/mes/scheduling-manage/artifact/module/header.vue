<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.productionLineTypeEnum"
      :options="artifactProductLineEnum.ENUM"
      type="enum"
      size="small"
      showOptionAll
      class="filter-item"
    />
    <tag-tabs
      v-model="query.structureClassId"
      class="filter-item"
      :style="'width:calc(100% - 205px)'"
      :data="artifactTypeList"
      itemKey="structureClassId"
      @change="crud.toQuery"
    >
      <template #default="{ item }">
        <span>{{ item.name }}：</span>
        <span>{{ item.quantity }}件</span>
      </template>
    </tag-tabs>
    <product-type-query :productType="productType" :toQuery="crud.toQuery" :query="query" />
    <rrOperation />
  </div>
  <crudOperation>
    <template #optLeft>
      <slot name="optLeft" />
    </template>
    <template #viewLeft>
      <slot name="viewLeft" />
    </template>
  </crudOperation>
</template>

<script setup>
import { getArtifactType } from '@/api/mes/scheduling-manage/artifact'
import { inject, watch, defineExpose } from 'vue'

import { artifactProductLineEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import useGetArtifactTypeList from '@compos/mes/scheduling/use-get-artifact-type-list'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import tagTabs from '@comp-common/tag-tabs'
import productTypeQuery from '@comp-mes/header-query/product-type-query'

const defaultQuery = {}

const productType = inject('productType')

const { crud, query } = regHeader(defaultQuery)

const { artifactTypeList, refreshArtifactType } = useGetArtifactTypeList({ getApi: getArtifactType, initHook: artifactTypeInit })

watch(
  [() => query.productionLineTypeEnum, () => crud.query.areaIdList],
  () => {
    console.log(crud.query.areaIdList, 'watch_areaIdList')
    refreshTypeList()
  },
  { deep: true, immediate: true }
)

// watch([() => query.structureClassId], () => {
//   crud.toQuery()
// })

function artifactTypeInit() {
  query.structureClassId = artifactTypeList.value?.length ? artifactTypeList.value[0].structureClassId : undefined
  crud.toQuery()
}

function refreshTypeList() {
  refreshArtifactType({
    productionLineTypeEnum: query.productionLineTypeEnum,
    areaIdList: crud.query.areaIdList
  })
}

defineExpose({
  refreshTypeList
})
</script>
