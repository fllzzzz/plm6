<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-if="lineTypeLoad && unshowLineType.length !== artifactProductLineEnum.KEYS.length"
      v-model="query.productionLineTypeEnum"
      :options="artifactProductLineEnum.ENUM"
      type="enum"
      :unshowVal="unshowLineType"
      size="small"
      default
      class="filter-item"
    />
    <tag-tabs
      v-if="artifactTypeList.length"
      v-model="query.structureClassId"
      class="filter-item"
      :style="'width:calc(100% - 205px)'"
      style="display: inline-block"
      :data="artifactTypeList"
      itemKey="structureClassId"
      @change="crud.toQuery"
    >
      <template #default="{ item }">
        <span>{{ item.name }}：</span>
        <span>{{ item.quantity }}件</span>
      </template>
    </tag-tabs>
    <div style="display: flex; justify-content: space-between">
      <div>
        <product-type-query :productType="productType" :toQuery="crud.toQuery" :query="query" />
        <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="crud.toQuery">搜索</common-button>
        <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
          重置
        </common-button>
      </div>
      <common-button class="filter-item" size="mini" type="info" @click.stop="productionLineStatus">车间在产数据</common-button>
    </div>
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
import { getArtifactType, getLineType } from '@/api/mes/scheduling-manage/artifact'
import { inject, watch, defineExpose, ref } from 'vue'
import { useRouter } from 'vue-router'
import { artifactProductLineEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import useGetArtifactTypeList from '@compos/mes/scheduling/use-get-artifact-type-list'
import crudOperation from '@crud/CRUD.operation'
import tagTabs from '@comp-common/tag-tabs'
import productTypeQuery from '@comp-mes/header-query/product-type-query'

const router = useRouter()
const defaultQuery = {}

const productType = inject('productType')
const unshowLineType = ref([])
const lineTypeLoad = ref(false)

const { crud, query } = regHeader(defaultQuery)

const { artifactTypeList, refreshArtifactType } = useGetArtifactTypeList({ getApi: getArtifactType, initHook: artifactTypeInit }, true)

watch(
  [() => query.productionLineTypeEnum, () => crud.query.areaIdList],
  () => {
    refreshTypeList()
  },
  { deep: true, immediate: true }
)

watch(
  [() => crud.query.areaIdList],
  () => {
    fetchLineType()
  },
  { deep: true, immediate: true }
)

// watch([() => query.structureClassId], () => {
//   crud.toQuery()
// })

function resetQuery() {
  query.name = undefined
  query.serialNumber = undefined
  query.specification = undefined
  query.material = undefined
  crud.toQuery()
}

function artifactTypeInit() {
  query.structureClassId = artifactTypeList.value?.length ? artifactTypeList.value[0].structureClassId : undefined
  crud.toQuery()
}

async function fetchLineType() {
  const areaIdList = crud.query.areaIdList
  query.productionLineTypeEnum = undefined
  unshowLineType.value = []
  lineTypeLoad.value = false
  if (!areaIdList?.length) return
  try {
    const { content } = await getLineType({ areaIdList })
    for (const item in artifactProductLineEnum.ENUM) {
      if (content.indexOf(artifactProductLineEnum[item].V) === -1) {
        unshowLineType.value.push(artifactProductLineEnum[item].V)
      }
    }
  } catch (er) {
    console.log('获取产线类型失败')
  } finally {
    lineTypeLoad.value = true
  }
}

function refreshTypeList() {
  query.structureClassId = undefined
  refreshArtifactType({
    productionLineTypeEnum: query.productionLineTypeEnum,
    areaIdList: crud.query.areaIdList
  })
}

function productionLineStatus() {
  router.push({ name: 'MesMonitoringKanban' })
}

defineExpose({
  refreshTypeList
})
</script>
