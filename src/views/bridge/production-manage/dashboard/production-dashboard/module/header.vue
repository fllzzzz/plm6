<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <monomer-select-area-tabs :project-id="globalProjectId" @change="fetchMonomerAndArea" />
      <common-radio-button
       v-if="showComponent"
        v-model="query.productType"
        :options="bridgeComponentTypeEnum.ENUM"
        size="small"
        class="filter-item"
        type="enumSL"
        default
        :unshowVal="[bridgeComponentTypeEnum.AUXILIARY_MATERIAL.V]"
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
import { ref, defineExpose, defineEmits } from 'vue'

import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import { mapGetters } from '@/store/lib'

import useUnshowProductTypeByMode from '@compos/use-bridge-unshow-productType-by-mode.js'
import useDashboardHeader from '@compos/bridge/dashboard/use-dashboard-header'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import ColorCard from '@comp/ColorCard'
import Scale from '@comp/Scale'
import productTypeQuery from '@comp-bridge/header-query/product-type-query'
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
const { unshowVal, showComponent } = useUnshowProductTypeByMode({ resetQuery: function () {
  query.productType = undefined
} })

const { globalProjectId } = mapGetters(['globalProjectId'])

const emit = defineEmits('load')

const boxScale = ref(1)
const { colors, boxZoomOut, getColor } = useDashboardHeader({ colorCardTitles: ['未生产', '生产中', '已完成'], emit, crud })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.detailLoading = false
    v.hasDetail = false
    v.compareQuantity = v.quantity
    v.isProcess = v.inProductionQuantity > 0
    v.boxColor = getColor(v, { quantity: 'completeQuantity', compare: 'compareQuantity' })
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
