<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <mat-header-query :basic-class="query.basicClass" :query="query" :to-query="crud.toQuery">
        <template #afterProjectWarehouseType>
          <common-radio-button
            v-model="query.basicClass"
            :options="rawMatClsEnum.ENUM"
            show-option-all
            type="enum"
            size="small"
            class="filter-item"
            @change="handleBasicClassChange"
          />
        </template>
        <template #afterWarehouse>
          <el-date-picker
            v-model="query.createTime"
            :default-time="defaultTime"
            type="daterange"
            range-separator=":"
            size="small"
            value-format="x"
            :shortcuts="PICKER_OPTIONS_SHORTCUTS"
            unlink-panels
            start-placeholder="买入开始日期"
            end-placeholder="买入结束日期"
            style="width: 270px"
            class="filter-item"
            @change="crud.toQuery"
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
import { ref } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import useGlobalProjectIdChangeToQuery from '@/composables/use-global-project-id-change-to-query'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  createTime: [], // [借用开始日期，借用结束日期]
  projectId: { value: undefined, resetAble: false }, // 原项目id
  basicClass: { value: undefined, resetAble: false }
}

const { crud, query } = regHeader(defaultQuery)
useGlobalProjectIdChangeToQuery(crud)

// 基础类型发生变化
async function handleBasicClassChange(val) {
  await crud.resetQuery()
  crud.data = []
  crud.setColumns()
}
</script>
