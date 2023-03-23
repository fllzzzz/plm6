<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <monomer-select-area-tabs :productType="query.productType" needConvert :project-id="globalProjectId" @change="fetchMonomerAndArea" />
      <factory-select v-model="query.factoryId" clearable class="filter-item" style="width: 200px" @change="crud.toQuery" />
      <common-select
        v-model="query.classificationId"
        :options="artifactTypeList"
        type="other"
        class="filter-item"
        :clearable="true"
        :dataStructure="{ key: 'structureClassId', label: 'name', value: 'structureClassId' }"
        :placeholder="query.processType === processMaterialListTypeEnum.ARTIFACT.V ? '选择构件类型' : '选择部件类型'"
        style="width: 170px"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.specification"
        size="small"
        placeholder="输入规格搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.material"
        size="small"
        placeholder="输入材质搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optRight>
        <common-select
          v-model="query.processType"
          :options="[processMaterialListTypeEnum.ARTIFACT, processMaterialListTypeEnum.ASSEMBLE]"
          type="enum"
          size="small"
          clearable
          placeholder="请选择工序类型"
          class="filter-item"
          style="width: 200px"
          @change="crud.toQuery"
        />
      </template>
      <template #viewLeft>
        <el-tag hit effect="plain" class="filter-item" type="info">
          <span style="color: #303133">制造上报数量</span>
          <span> / </span>
          <span style="color: #1682e6">质检数量</span>
        </el-tag>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { inject } from 'vue'
import { mapGetters } from '@/store/lib'
import { regHeader } from '@compos/use-crud'
import { processMaterialListTypeEnum } from '@enum-ms/mes'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelectAreaTabs from '@comp-base/monomer-select-area-tabs'
import factorySelect from '@comp-base/factory-select'

const artifactTypeList = inject('artifactTypeList')

const defaultQuery = {
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  factoryId: { value: undefined, resetAble: false },
  classificationId: undefined,
  serialNumber: undefined,
  specification: undefined,
  material: undefined,
  processType: processMaterialListTypeEnum.ARTIFACT.V
}
const { crud, query } = regHeader(defaultQuery)

const { globalProjectId } = mapGetters(['globalProjectId'])

function fetchMonomerAndArea({ monomerId, areaId }) {
  query.monomerId = monomerId
  query.areaId = areaId
  crud.toQuery()
}
</script>
