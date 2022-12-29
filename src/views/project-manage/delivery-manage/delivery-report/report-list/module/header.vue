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
        :options="globalProject.projectType === projectTypeEnum.STEEL.V?installProjectTypeEnum.ENUM:[bridgeComponentTypeEnum.BOX,bridgeComponentTypeEnum.AUXILIARY_MATERIAL]"
        type="enum"
        class="filter-item"
        @change="productTypeChange"
      />
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="query.projectId"
        :main-product-type="(globalProject.projectType === projectTypeEnum.STEEL.V && query.productType!==installProjectTypeEnum.AUXILIARY.V)?query.productType:''"
        :default="false"
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

import moment from 'moment'
import { regHeader } from '@compos/use-crud'
import { projectTypeEnum } from '@enum-ms/contract'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'
import { installProjectTypeEnum } from '@enum-ms/project'
import { manufactureTypeEnum } from '@enum-ms/plan'

import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'

const defaultQuery = {
  projectId: { value: undefined, resetAble: false },
  date: undefined,
  startDate: undefined,
  endDate: undefined,
  productType: props.globalProject.projectType === projectTypeEnum.STEEL.V ? installProjectTypeEnum.ARTIFACT.V : bridgeComponentTypeEnum.BOX.V,
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

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  globalProject: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
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
  crud.toQuery()
}
</script>
