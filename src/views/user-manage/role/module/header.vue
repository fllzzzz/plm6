<template>
  <div>
    <div v-show="crud.searchToggle">
      <el-input
        v-model="query.name"
        placeholder="按角色名称搜索"
        class="filter-item"
        style="width: 200px"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
       <template #viewLeft>
        <export-button :fn="downloadRoleUser" class="filter-item" v-permission="crud.permission.get" :disabled="crud.data.length === 0">
          人员设置清单
        </export-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { downloadRoleUser } from '@/api/user-manage/role'
import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import ExportButton from '@comp-common/export-button/index.vue'

const defaultQuery = {
  name: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>
