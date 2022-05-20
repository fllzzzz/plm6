<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <mat-header-query
        :basic-class="query.basicClass"
        :show-material-is-whole="false"
        :show-warehouse="false"
        :show-basic-class-query="false"
        :query="query"
        :to-query="crud.toQuery"
      >
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
            v-model="query.returnTime"
            :default-time="defaultTime"
            type="daterange"
            range-separator=":"
            size="small"
            value-format="x"
            :shortcuts="PICKER_OPTIONS_SHORTCUTS"
            unlink-panels
            start-placeholder="开始日期"
            end-placeholder="结束日期"
            style="width: 270px"
            class="filter-item"
            @change="crud.toQuery"
          />
        </template>
        <template #beforeWarehouse>
          <project-cascader
            v-model="query.projectId"
            placeholder="所属项目"
            clearable
            @change="crud.toQuery"
            class="filter-item"
            style="width: 300px"
          />
        </template>
        <template #secondLineLastItem>
          <el-input
            v-model.trim="query.serialNumber"
            clearable
            style="width: 200px"
            size="small"
            placeholder="红冲单号"
            class="filter-item"
            @keyup.enter="crud.toQuery"
          />
        </template>
      </mat-header-query>
      <rrOperation />
    </div>
    <crudOperation>
      <!-- 打印 -->
      <template #optLeft>
        <print-table
          v-permission="permission.print"
          api-key="wmsRmSupplementReceipt"
          :params="crud.query"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { inject, ref } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'

const permission = inject('permission')
const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  returnTime: [], // [开始日期，结束日期]
  serialNumber: undefined, // 红冲单号
  projectId: { value: undefined, resetAble: false }, // 项目id
  factoryId: { value: undefined, resetAble: false }, // 工厂id
  basicClass: { value: undefined, resetAble: false }
}

const { crud, query } = regHeader(defaultQuery)

// 基础类型发生变化
async function handleBasicClassChange(val) {
  await crud.resetQuery()
  crud.data = []
  crud.setColumns()
}
</script>
