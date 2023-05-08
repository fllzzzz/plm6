<template>
  <div v-show="crud.searchToggle">
    <common-radio-button
      v-model="query.issueStatusEnum"
      :options="issueStatusEnum.ENUM"
      :unshowVal="[issueStatusEnum.NOT_NESTING.V]"
      showOptionAll
      type="enum"
      size="small"
      class="filter-item"
      @change="crud.toQuery"
    />
    <div>
      <!-- <monomer-select
        ref="monomerRef"
        v-model="query.monomerId"
        clearable
        :default="false"
        :project-id="crud.query.projectId"
        class="filter-item"
        @change="crud.toQuery"
      /> -->
      <el-input
        v-model="query.orderNumber"
        placeholder="任务单号搜索"
        class="filter-item"
        style="width: 160px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.thick"
        placeholder="零件厚度搜索"
        class="filter-item"
        style="width: 160px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.material"
        placeholder="零件材质搜索"
        class="filter-item"
        style="width: 160px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button>
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import { machinePartSchedulingIssueStatusEnum as issueStatusEnum } from '@enum-ms/mes'
// import monomerSelect from '@/components-system/plan/monomer-select'

const defaultQuery = {}

function searchQuery() {
  crud.toQuery()
}
function resetQuery() {
  query.issueStatusEnum = undefined
  // query.monomerId = undefined
  query.orderNumber = undefined
  query.thick = undefined
  query.material = undefined
  crud.toQuery()
}

const { crud, query } = regHeader(defaultQuery)
</script>
