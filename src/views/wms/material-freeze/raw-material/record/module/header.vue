<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <!-- 物料查询相关 -->
      <mat-header-query :basic-class="query.basicClass" :query="query" :to-query="crud.toQuery" show-project-warehouse-type>
        <template #firstItem>
          <common-radio-button
            v-model="query.freezeType"
            :options="materialFreezeTypeEnum.ENUM"
            show-option-all
            type="enum"
            size="small"
            class="filter-item"
            @change="crud.toQuery"
          />
        </template>
        <template #afterProjectWarehouseType>
          <common-radio-button
            v-model="query.basicClass"
            :options="rawMatClsEnum.ENUM"
            type="enum"
            size="small"
            class="filter-item"
            @change="handleBasicClassChange"
          />
        </template>
      </mat-header-query>
      <rr-operation />
    </div>
    <crud-operation />
  </div>
</template>

<script setup>
import { watch } from 'vue'
import { mapGetters } from '@/store/lib'
import { rawMatClsEnum, steelClsEnum } from '@/utils/enum/modules/classification'
import { projectWarehouseTypeEnum, materialFreezeTypeEnum } from '@/utils/enum/modules/wms'

import { regHeader } from '@compos/use-crud'
import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'

// 查询参数
const defaultQuery = {
  projectId: { value: undefined, resetAble: false }, // 项目id
  projectWarehouseType: { value: projectWarehouseTypeEnum.PUBLIC.V, resetAble: false }, // 仓库类型
  basicClass: { value: steelClsEnum.STEEL_PLATE.V, resetAble: false } // 基础分类
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
