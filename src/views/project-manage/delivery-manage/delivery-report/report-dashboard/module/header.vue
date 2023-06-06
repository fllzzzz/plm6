<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.productType"
        :options="installTypeEnumArr"
        type="enum"
        class="filter-item"
        @change="productTypeChange"
      />
      <monomer-select
        v-if="query.productType!==installProjectTypeEnum.ENCLOSURE.V"
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="query.projectId"
        :main-product-type="(globalProject.projectType === projectTypeEnum.STEEL.V && query.productType!==installProjectTypeEnum.AUXILIARY.V)? query.productType:''"
        clearable
        class="filter-item"
        @change="crud.toQuery"
        @getAreaInfo="getAreaInfo"
      />
       <common-select
        v-if="query.productType!==installProjectTypeEnum.AUXILIARY.V && query.productType!==installProjectTypeEnum.ENCLOSURE.V"
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
      <template v-if="query.productType===installProjectTypeEnum.ENCLOSURE.V">
         <common-select
          v-model="query.category"
          :options="TechnologyTypeAllEnum.ENUM"
          :unshow-options="[TechnologyTypeAllEnum.STRUCTURE.K,TechnologyTypeAllEnum.BRIDGE.K]"
          type="enum"
          size="small"
          clearable
          placeholder="请选择围护类型"
          class="filter-item"
          style="width:200px;"
          @change="categoryChange"
        />
        <common-select
          v-model="query.areaId"
          :options="areaInfo"
          type="other"
          :dataStructure="typeProp"
          size="small"
          clearable
          placeholder="请选择批次"
          class="filter-item"
          style="width:200px;"
          @change="crud.toQuery"
        />
      </template>
    </div>
    <crudOperation :show-grid="false" :show-refresh="false">
      <template #optRight>
        <color-card class="filter-item" v-model:value="query.receivingStatus" :colors="colors" color-border select-able @change="crud.toQuery" />
      </template>
      <template #viewLeft>
        <scale class="filter-item" v-model:value="boxScale" :intervals="400" @zoom-out="boxZoomOut" />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, defineExpose, defineEmits, defineProps, watch } from 'vue'
import { allProjectPlan } from '@/api/enclosure/enclosure-plan/area'

import { businessTypeEnum, TechnologyTypeAllEnum } from '@enum-ms/contract'
import { projectTypeEnum } from '@enum-ms/contract'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import { installProjectTypeEnum } from '@enum-ms/project'

import useDashboardHeader from '@compos/mes/dashboard/use-dashboard-header'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import ColorCard from '@comp/ColorCard'
import Scale from '@comp/Scale'
import monomerSelect from '@/components-system/plan/monomer-select'

import { mapGetters } from '@/store/lib'

const { installTypeEnumArr } = mapGetters('installTypeEnumArr')

const props = defineProps({
  globalProject: {
    type: Object,
    default: () => {}
  },
  projectId: {
    type: [Number, String],
    default: undefined
  }
})
const defaultQuery = {
  productType: props.globalProject.projectType === projectTypeEnum.STEEL.V ? installProjectTypeEnum.ARTIFACT.V : bridgeComponentTypeEnum.BOX.V,
  monomerId: { value: undefined, resetAble: false },
  category: undefined,
  areaId: { value: undefined, resetAble: false },
  receivingStatus: undefined
}
const { crud, query, CRUD } = regHeader(defaultQuery)

const emit = defineEmits('load')

const boxScale = ref(1)
const typeProp = { key: 'id', label: 'name', value: 'id' }
const totalArea = ref([])
const areaInfo = ref([])

const { colors, boxZoomOut, getColor } = useDashboardHeader({
  colorCardTitles: ['未收货', '部分收货', '全部收货'],
  emit,
  crud
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      totalArea.value = []
      if (crud.query.productType === installProjectTypeEnum.ENCLOSURE.V) {
        getAllProjectPlan()
      }
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = props.globalProject.businessType === businessTypeEnum.INSTALLATION.V ? props.globalProject.id : undefined
  return !!crud.query.projectId
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.boxColor = getColor(v, { quantity: 'receivingQuantity', compare: 'quantity' })
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
  if (val === installProjectTypeEnum.ENCLOSURE.V) {
    getAllProjectPlan()
  }
  crud.toQuery()
}

function categoryChange(val) {
  areaInfo.value = totalArea.value?.filter(v => v.category === val) || []
  crud.toQuery()
}

async function getAllProjectPlan() {
  crud.query.monomerId = undefined
  areaInfo.value = []
  if (props.projectId) {
    try {
      const data = await allProjectPlan(props.projectId) || []
      totalArea.value = data || []
      if (crud.query.category) {
        areaInfo.value = totalArea.value?.filter(v => v.category === crud.query.category)
      } else {
        areaInfo.value = totalArea.value
      }
    } catch (e) {
      console.log('获取项目所有围护计划', e)
    }
  }
}

defineExpose({
  boxScale
})
</script>
