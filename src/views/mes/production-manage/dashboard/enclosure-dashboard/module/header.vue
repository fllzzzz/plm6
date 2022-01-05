<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <monomer-select-area-tabs :productType="query.productType" needConvert :project-id="globalProjectId" @change="fetchMonomerAndArea" />
      <!-- <common-radio-button
        type="enum"
        v-model="query.category"
        :options="mesEnclosureTypeEnum.ENUM"
        showOptionAll
        placeholder="请选择围护类型"
        class="filter-item"
        @change="crud.toQuery"
      /> -->
      <factory-select
        v-model="query.factoryId"
        class="filter-item"
        style="width: 200px"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.name"
        size="small"
        placeholder="输入名称搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.plate"
        size="small"
        placeholder="输入板型搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.color"
        size="small"
        placeholder="输入颜色搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation :show-grid="false" :show-refresh="false">
      <template #optRight>
        <color-card
          class="filter-item"
          v-model:value="query.status"
          :colors="colors"
          color-border
          select-able
          @change="crud.toQuery"
        />
      </template>
      <template #viewLeft>
        <scale class="filter-item" v-model:value="boxScale" :intervals="400" @zoom-out="boxZoomOut" />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, defineExpose, defineEmits } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'
import { mapGetters } from '@/store/lib'

import useDashboardHeader from '@compos/mes/dashboard/use-dashboard-header'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import ColorCard from '@comp/ColorCard'
import Scale from '@comp/Scale'
import monomerSelectAreaTabs from '@comp-base/monomer-select-area-tabs'
import factorySelect from '@comp-base/factory-select'

const defaultQuery = {
  name: '',
  serialNumber: '',
  plate: '',
  color: '',
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  factoryId: { value: undefined, resetAble: false },
  status: { value: undefined, resetAble: false },
  productType: componentTypeEnum.ENCLOSURE.V
}
const { crud, query, CRUD } = regHeader(defaultQuery)

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
