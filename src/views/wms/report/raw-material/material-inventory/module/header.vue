<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <common-radio-button
        type="enum"
        v-model="query.basicClass"
        :options="rawMatClsEnum.ENUM"
        show-option-all
        clearable
        class="filter-item"
        @change="crud.toQuery"
      />
      <material-cascader
        v-model="query.classifyId"
        :basic-class="query.basicClass"
        separator=" > "
        check-strictly
        show-all-levels
        clearable
        size="small"
        class="filter-item"
        style="width: 300px"
        placeholder="可选择/输入科目、编号搜索"
        @change="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optLeft>
        <export-button v-permission="permission.download" :params="query" :fn="excel" response-header-result>
          下载库存报表（根据查询条件）
        </export-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { excel } from '@/api/wms/report/raw-material/inventory'
import { inject } from 'vue'
import { rawMatClsEnum } from '@enum-ms/classification'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import MaterialCascader from '@comp-cls/material-cascader/index.vue'
import ExportButton from '@comp-common/export-button/index.vue'

const defaultQuery = {
  basicClass: undefined, // 采购类型
  classifyId: undefined // 科目id
}

const permission = inject('permission')
const { crud, query } = regHeader(defaultQuery)
</script>
