<template>
  <div v-show="crud.searchToggle">
    <!-- <common-radio-button
      v-if="lineTypeLoad && unshowLineType.length !== artifactProductLineEnum.KEYS.length"
      v-model="query.productionLineTypeEnum"
      :options="artifactProductLineEnum.ENUM"
      type="enum"
      :unshowVal="unshowLineType"
      size="small"
      default
      class="filter-item"
    /> -->
    <tag-tabs
      v-if="boxTypeList.length"
      v-model="query.structureClassId"
      class="filter-item"
      :style="'width:100%;'"
      style="display: inline-block"
      :data="boxTypeList"
      itemKey="structureClassId"
      @change="crud.toQuery"
    >
      <template #default="{ item }">
        <span>{{ item.name }}：</span>
        <span>{{ item.quantity }}件</span>
      </template>
    </tag-tabs>
    <product-type-query :productType="productType" :toQuery="crud.toQuery" :query="query" />
    <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="crud.toQuery">搜索</common-button>
    <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
      重置
    </common-button>
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
import { getBoxType } from '@/api/bridge/scheduling-manage/box'
import { inject, watch, defineExpose } from 'vue'

import { artifactProductLineEnum } from '@enum-ms/mes'

import { regHeader } from '@compos/use-crud'
import useGetBoxTypeList from '@compos/bridge/scheduling/use-get-box-type-list'
import crudOperation from '@crud/CRUD.operation'
import tagTabs from '@comp-common/tag-tabs'
import productTypeQuery from '@comp-bridge/header-query/product-type-query'

const defaultQuery = {
  productionLineTypeEnum: artifactProductLineEnum.TRADITION.V
}

const productType = inject('productType')
// const unshowLineType = ref([])
// const lineTypeLoad = ref(false)

const { crud, query } = regHeader(defaultQuery)

const { boxTypeList, refreshBoxType } = useGetBoxTypeList({ getApi: getBoxType, initHook: boxTypeInit }, true)

// watch(
//   [() => query.productionLineTypeEnum, () => crud.query.areaIdList],
//   () => {
//     refreshTypeList()
//   },
//   { deep: true, immediate: true }
// )

watch(
  [() => crud.query.areaIdList],
  () => {
    refreshTypeList()
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

function boxTypeInit() {
  query.structureClassId = boxTypeList.value?.length ? boxTypeList.value[0].structureClassId : undefined
  crud.toQuery()
}

// async function fetchLineType() {
//   const areaIdList = crud.query.areaIdList
//   query.productionLineTypeEnum = undefined
//   unshowLineType.value = []
//   lineTypeLoad.value = false
//   if (!areaIdList?.length) return
//   try {
//     const { content } = await getLineType({ areaIdList })
//     for (const item in artifactProductLineEnum.ENUM) {
//       if (content.indexOf(artifactProductLineEnum[item].V) === -1) {
//         unshowLineType.value.push(artifactProductLineEnum[item].V)
//       }
//     }
//   } catch (er) {
//     console.log('获取产线类型失败')
//   } finally {
//     lineTypeLoad.value = true
//   }
// }

function refreshTypeList() {
  query.structureClassId = undefined
  refreshBoxType({
    productionLineTypeEnum: query.productionLineTypeEnum,
    areaIdList: crud.query.areaIdList
  })
}

defineExpose({
  refreshTypeList
})
</script>
