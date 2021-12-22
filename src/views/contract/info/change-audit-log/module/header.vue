<template>
  <div>
    <div v-show="crud.searchToggle">
      <common-select
        v-model="query.type"
        :options="contractChangeTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        placeholder="变更类型"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-select
        v-model="query.source"
        :options="systemTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="来源"
        style="width:200px"
        @change="crud.toQuery"
      />
      <common-select
        v-model="query.auditStatus"
        :options="auditTypeEnum.ENUM"
        type="enum"
        size="small"
        clearable
        class="filter-item"
        placeholder="状态"
        style="width:200px"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.leader"
        placeholder="负责人"
        style="width:200px"
        class="filter-item"
      />
      <rrOperation/>
    </div>
  </div>
</template>

<script setup>
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import { auditTypeEnum, contractChangeTypeEnum, systemTypeEnum } from '@enum-ms/contract'

const defaultQuery = {
  projectId: { value: undefined, resetAble: false },
  auditStatus: undefined,
  leader: undefined,
  source: undefined,
  type: undefined
}
const { crud, query } = regHeader(defaultQuery)
</script>
