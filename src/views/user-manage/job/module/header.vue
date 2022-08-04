<template>
  <div>
    <div v-show="crud.searchToggle">
        <el-input
        v-model="query.name"
        placeholder="按岗位名称搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @blur="crud.toQuery"
      />
      <common-radio-button
        v-model="query.enabled"
        :options="systemEnabledEnum.ENUM"
        type="enum"
        showOptionAll
        class="filter-item"
        size="small"
        @change="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
       <template #viewLeft>
        <export-button :fn="downloadJobUser" class="filter-item" :params="{enabled:query.enabled}" v-permission="crud.permission.get" :disabled="crud.data.length === 0">
          人员设置清单
        </export-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { downloadJobUser } from '@/api/user-manage/job'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { systemEnabledEnum } from '@enum-ms/system'
import ExportButton from '@comp-common/export-button/index.vue'

const defaultQuery = {
  name: undefined,
  enabled: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
