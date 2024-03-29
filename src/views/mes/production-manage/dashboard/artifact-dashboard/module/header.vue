<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <monomer-select-area-tabs :productType="query.productType" needConvert :project-id=" globalProjectId" @change="fetchMonomerAndArea" />
      <common-radio-button
       v-if="showComponent"
        v-model="query.productType"
        :options="componentTypeEnum.ENUM"
        size="small"
        class="filter-item"
        type="enumSL"
        default
        :unshowVal="[componentTypeEnum.ENCLOSURE.V,componentTypeEnum.AUXILIARY_MATERIAL.V]"
        @change="crud.toQuery"
      />
      <factory-select v-model="query.factoryId" clearable class="filter-item" style="width: 200px" @change="crud.toQuery" />
      <product-type-query :productType="query.productType" :toQuery="crud.toQuery" :query="query" />
      <rrOperation />
    </div>
    <crudOperation :show-grid="false" :show-refresh="false">
      <template #optRight>
        <color-card class="filter-item" v-model:value="query.status" :colors="colors" color-border select-able @change="crud.toQuery" />
      </template>
      <template #viewLeft>
        <scale class="filter-item" v-model:value="boxScale" :intervals="400" @zoom-out="boxZoomOut" />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, watch, defineExpose, defineEmits } from 'vue'
import { componentTypeEnum } from '@enum-ms/mes'
import { mapGetters } from '@/store/lib'
import useUnshowProductTypeByMode from '@compos/use-unshow-productType-by-mode.js'
import useStructureDashboardHeader from '@compos/mes/dashboard/use-structure-dashboard-header'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import ColorCard from '@comp/ColorCard'
import Scale from '@comp/Scale'
import productTypeQuery from '@comp-mes/header-query/product-type-query'
import monomerSelectAreaTabs from '@comp-base/monomer-select-area-tabs'
import factorySelect from '@comp-base/factory-select'

const defaultQuery = {
  name: undefined,
  serialNumber: undefined,
  specification: undefined,
  material: undefined,
  status: { value: undefined, resetAble: false },
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  factoryId: { value: undefined, resetAble: false }
}
const { crud, query, CRUD } = regHeader(defaultQuery)
const { unshowVal, showComponent } = useUnshowProductTypeByMode({
  resetQuery: function () {
    // query.productType = undefined
  }
})

const { globalProjectId } = mapGetters(['globalProjectId'])

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      query.projectId = val
    }
  }, { immediate: true }
)
const emit = defineEmits('load')

const boxScale = ref(1)
const { colors, boxZoomOut, getColor } = useStructureDashboardHeader({
  colorCardTitles: ['未生产', '生产中', '已完成', '待质检'],
  emit,
  crud
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.detailLoading = false
    v.hasDetail = false
    v.compareQuantity = v.quantity
    v.isProcess = v.completeQuantity > 0
    v.boxColor = getColor(v, { complete: 'completeQuantity', listQuantity: 'quantity', inProduction: 'inProductionQuantity' })
    return v
  })
}

function fetchMonomerAndArea({ monomerId, areaId }) {
  query.monomerId = monomerId
  query.areaId = areaId
  crud.toQuery()
}

defineExpose({
  boxScale
})
</script>
