<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.boolAmendStatus"
        :options="artifactSpecReviseEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="query.projectId"
        :productType="TechnologyTypeAllEnum.STRUCTURE.V"
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
        :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
        size="small"
        clearable
        placeholder="请选择区域"
        class="filter-item"
        style="width: 200px"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.name"
        size="small"
        placeholder="输入名称搜索"
        style="width: 170px; margin-left: 0"
        class="filter-item"
        clearable
      />
      <el-input v-model="query.serialNumber" size="small" placeholder="输入编号搜索" style="width: 170px" class="filter-item" clearable />

      <el-input
        v-if="query.boolAmendStatus !== artifactSpecReviseEnum.REVISED.V"
        v-model="query.oldSpecification"
        size="small"
        placeholder="输入规格搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <el-input
        v-else
        v-model="query.newSpecification"
        size="small"
        placeholder="输入规格搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <rrOperation />
    </div>
    <tag-tabs
      v-if="specificationList?.length"
      v-model="query.specification"
      class="filter-item"
      :style="'width:85%; float: left'"
      :data="specificationList"
      itemKey="id"
      @change="crud.toQuery"
    >
      <template #default="{ item }">
        <span>{{ item.name }}</span>
      </template>
    </tag-tabs>
    <common-button
      style="float: right"
      type="primary"
      :disabled="crud.selections.length === 0"
      @click="changeVisible = true"
      v-permission="crud.permission.edit"
    >
      批量修改
    </common-button>
    <changeTable v-model="changeVisible" :list="crud.selections" @success="crud.toQuery" />
  </div>
</template>

<script setup>
import { ref, watch, inject } from 'vue'
import { regHeader } from '@compos/use-crud'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { artifactSpecReviseEnum } from '@enum-ms/mes'
import { mapGetters } from '@/store/lib'

import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import changeTable from './change-table'
import tagTabs from '@comp-common/tag-tabs'

const specificationList = inject('specificationList')
const defaultQuery = {
  boolAmendStatus: artifactSpecReviseEnum.NOT.V,
  projectId: { value: undefined, resetAble: false },
  name: undefined,
  serialNumber: undefined,
  newSpecification: undefined,
  oldSpecification: undefined,
  monomerId: undefined,
  areaId: undefined,
  specification: undefined
}

const monomerSelectRef = ref()
const areaInfo = ref([])
const changeVisible = ref(false)
const { globalProjectId } = mapGetters(['globalProjectId'])
const { crud, query } = regHeader(defaultQuery)

watch(
  () => globalProjectId,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function getAreaInfo(val) {
  areaInfo.value = val || []
}
</script>
