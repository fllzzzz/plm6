<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.productType"
        :options="globalProject.projectType === projectTypeEnum.STEEL.V?installTypeEnumArr:[bridgeComponentTypeEnum.BOX,bridgeComponentTypeEnum.AUXILIARY_MATERIAL]"
        type="enum"
        class="filter-item"
        @change="productTypeChange"
      />
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="query.projectId"
        :main-product-type="(globalProject.projectType === projectTypeEnum.STEEL.V && query.productType!==installProjectTypeEnum.AUXILIARY.V)?query.productType:''"
        clearable
        class="filter-item"
        @change="crud.toQuery"
        @getAreaInfo="getAreaInfo"
      />
       <common-select
        v-model="query.areaId"
        :options="areaInfo"
        type="other"
        :dataStructure="typeProp"
        size="small"
        clearable
        placeholder="请选择区域"
        class="filter-item"
        style="width:200px;"
        @change="crud.toQuery"
      />
    </div>
    <crudOperation :show-grid="false" :show-refresh="false">
      <template #optRight>
        <color-card class="filter-item" v-model:value="query.installStatus" :colors="colors" color-border select-able @change="crud.toQuery" />
      </template>
      <template #viewLeft>
        <scale class="filter-item" v-model:value="boxScale" :intervals="400" @zoom-out="boxZoomOut" />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, defineExpose, defineEmits, defineProps } from 'vue'

import { projectTypeEnum } from '@enum-ms/contract'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import { businessTypeEnum } from '@enum-ms/contract'
import { installProjectTypeEnum } from '@enum-ms/project'

import useDashboardHeader from '@compos/mes/dashboard/use-dashboard-header'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import ColorCard from '@comp/ColorCard'
import Scale from '@comp/Scale'
import monomerSelect from '@/components-system/plan/monomer-select'
import { mapGetters } from '@/store/lib'

const { installTypeEnumArr } = mapGetters('installTypeEnumArr')

const defaultQuery = {
  productType: props.globalProject.projectType === projectTypeEnum.STEEL.V ? installProjectTypeEnum.ARTIFACT.V : bridgeComponentTypeEnum.BOX.V,
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  installStatus: undefined
}
const { crud, query, CRUD } = regHeader(defaultQuery)

const emit = defineEmits('load')

const boxScale = ref(1)
const typeProp = { key: 'id', label: 'name', value: 'id' }
const areaInfo = ref([])

const props = defineProps({
  globalProject: {
    type: Object,
    default: () => {}
  }
})

const { colors, boxZoomOut, getColor } = useDashboardHeader({
  colorCardTitles: ['未安装', '部分安装', '全部安装'],
  emit,
  crud
})

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = props.globalProject.businessType === businessTypeEnum.INSTALLATION.V ? props.globalProject.id : undefined
  return !!crud.query.projectId
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.boxColor = getColor(v, { quantity: 'installQuantity', compare: 'quantity' })
    return v
  })
}

function getAreaInfo(val) {
  areaInfo.value = val || []
  if (areaInfo.value.length > 0) {
    crud.query.areaId = areaInfo.value[0].id
  }
}

function productTypeChange(val) {
  query.areaId = undefined
  crud.toQuery()
}

defineExpose({
  boxScale
})
</script>
