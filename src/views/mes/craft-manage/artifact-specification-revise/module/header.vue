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
        :project-id="props.projectId"
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
        style="width:200px;"
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
      <el-input
        v-model="query.serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <el-input
        v-model="query.specification"
        size="small"
        placeholder="输入规格搜索"
        style="width: 170px"
        class="filter-item"
        clearable
      />
      <rrOperation />
    </div>
    <crudOperation>
      <!-- <template #optLeft>
        <el-tag type="danger">未修正：{{crud.page.total}}件</el-tag>
      </template> -->
      <template #viewLeft>
        <common-button type="primary" :disabled="crud.selections.length===0" @click="changeVisible=true">批量修改</common-button>
      </template>
    </crudOperation>
    <changeTable v-model="changeVisible" :list="crud.selections" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { regHeader } from '@compos/use-crud'

import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { artifactSpecReviseEnum } from '@enum-ms/mes'

import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import changeTable from './change-table'

const defaultQuery = {
  boolAmendStatus: artifactSpecReviseEnum.NOT.V,
  name: '',
  serialNumber: '',
  specification: '',
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false }
}

const monomerSelectRef = ref()
const areaInfo = ref([])
const changeVisible = ref(false)
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

function getAreaInfo(val) {
  areaInfo.value = val || []
}
</script>