<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <mat-header-query
        :basic-class="query.basicClass"
        :query="query"
        :to-query="crud.toQuery"
        show-project-warehouse-type
        :show-material-is-whole="false"
      >
        <template #afterProjectWarehouseType>
          <common-radio-button
            v-model="query.basicClass"
            :options="steelClsEnum.ENUM"
            type="enum"
            size="small"
            class="filter-item"
            @change="handleBasicClassChange"
          />
        </template>
      </mat-header-query>

      <rrOperation />
    </div>
    <crudOperation>
      <!-- TODO:打印 -->
      <template #optLeft></template>
    </crudOperation>
  </div>
</template>

<script setup>
import { watch } from 'vue'
import { rawMatClsEnum, steelClsEnum } from '@/utils/enum/modules/classification'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import { mapGetters } from '@/store/lib'

const defaultQuery = {
  projectId: { value: undefined, resetAble: false }, // 原项目id
  basicClass: { value: rawMatClsEnum.STEEL_PLATE.V, resetAble: false }
}

const { CRUD, crud, query } = regHeader(defaultQuery)

// 全局项目id
const { globalProjectId } = mapGetters('globalProjectId')

// 选中项目库时， 根据项目id的变化刷新列表
watch(
  globalProjectId,
  () => {
    if (crud.query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V) {
      crud.toQuery()
    }
  },
  { immediate: true }
)

CRUD.HOOK.beforeToQuery = () => {
  if (crud.query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V) {
    crud.query.projectId = globalProjectId.value || undefined
  } else {
    crud.query.projectId = undefined
  }
}

// 基础类型发生变化
async function handleBasicClassChange(val) {
  await crud.resetQuery()
  crud.data = []
  crud.setColumns()
}
</script>
