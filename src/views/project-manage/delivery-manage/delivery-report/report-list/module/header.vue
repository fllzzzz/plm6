<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.date"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item date-item"
        start-placeholder="开始时间"
        end-placeholder="结束时间"
        style="width: 240px"
        @change="handleDateChange"
      />
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
        :main-product-type="query.productType"
        :default="false"
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
      <div>
        <el-input
          v-model.trim="query.supplierName"
          placeholder="供应商搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model.trim="query.serialNumber"
          placeholder="编号搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <el-input
          v-model.trim="query.name"
          placeholder="名称搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <common-select
          v-model="query.areaType"
          :options="manufactureTypeEnum.ENUM"
          type="enum"
          size="small"
          clearable
          placeholder="自制或外协"
          class="filter-item"
          style="width:200px;"
          @change="crud.toQuery"
        />
        <rrOperation />
      </div>
    </div>
    <crudOperation>
      <template #viewLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="deliveryReportList"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, defineProps, watch } from 'vue'
import { allProjectPlan } from '@/api/enclosure/enclosure-plan/area'

import moment from 'moment'
import { regHeader } from '@compos/use-crud'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { installProjectTypeEnum } from '@enum-ms/project'
import { manufactureTypeEnum } from '@enum-ms/plan'

import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import { mapGetters } from '@/store/lib'

const { installTypeEnumArr } = mapGetters('installTypeEnumArr')

const defaultQuery = {
  projectId: { value: undefined, resetAble: false },
  date: undefined,
  startDate: undefined,
  endDate: undefined,
  productType: installProjectTypeEnum.ARTIFACT.V,
  category: undefined,
  monomerId: undefined,
  areaId: undefined,
  name: undefined,
  serialNumber: undefined,
  supplierName: undefined,
  areaType: undefined
}

const { crud, query } = regHeader(defaultQuery)
const typeProp = { key: 'id', label: 'name', value: 'id' }
const areaInfo = ref([])
const totalArea = ref([])

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
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

// 时间变动
function handleDateChange() {
  if (query.date && query.date.length > 1) {
    query.startDate = moment(query.date[0]).valueOf()
    query.endDate = moment(query.date[1]).valueOf()
  } else {
    query.startDate = undefined
    query.endDate = undefined
  }
  crud.toQuery()
}

function getAreaInfo(val) {
  areaInfo.value = val || []
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
</script>
