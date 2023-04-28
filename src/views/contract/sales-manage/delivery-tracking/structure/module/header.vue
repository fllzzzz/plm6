<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <monomer-select
        v-model="query.monomerId"
        :project-id="searchQuery.projectId"
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
        v-model.trim="query.serialNumber"
        size="small"
        placeholder="输入编号"
        style="width: 200px;"
        class="filter-item"
        clearable
      />
      <el-input
        v-model.trim="query.specification"
        size="small"
        placeholder="输入规格"
        style="width: 200px;"
        class="filter-item"
        clearable
      />
      <el-input
        v-model.trim="query.material"
        size="small"
        placeholder="输入材质"
        style="width: 200px;"
        class="filter-item"
        clearable
      />
      <rrOperation/>
    </div>
    <crudOperation>
      <template #viewLeft>
        <!-- <print-table
          v-permission="crud.permission.print"
          api-key="saleOrderTracking"
          :params="{ ...query }"
          size="mini"
          type="warning"
          class="filter-item"
        /> -->
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, inject, nextTick, watch } from 'vue'
import moment from 'moment'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import monomerSelect from '@/components-system/plan/monomer-select'

const searchQuery = inject('searchQuery')

const defaultQuery = {
  startDate: undefined, endDate: undefined,
  projectContent: undefined,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)
const areaInfo = ref([])

watch(
  searchQuery,
  (val) => {
    nextTick(() => {
      crud.query.projectId = val.projectId
      if (searchQuery.date && searchQuery.date.length > 1) {
        query.startDate = moment(searchQuery.date[0]).valueOf()
        query.endDate = moment(searchQuery.date[1]).valueOf()
      } else {
        query.startDate = undefined
        query.endDate = undefined
      }
      crud.toQuery()
    })
  },
  { immediate: true }
)

function getAreaInfo(val) {
  areaInfo.value = val || []
}
</script>
