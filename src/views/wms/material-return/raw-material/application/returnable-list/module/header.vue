<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <mat-header-query :basic-class="query.basicClass" :query="query" :to-query="crud.toQuery">
        <template #firstItem>
          <el-tag class="filter-item" effect="plain" size="medium">出库材料信息</el-tag>
        </template>
        <template #afterProjectWarehouseType>
          <common-radio-button
            v-if="!isComponent"
            v-model="query.basicClass"
            :options="rawMatClsEnum.ENUM"
            type="enum"
            size="small"
            class="filter-item"
            @change="handleBasicClassChange"
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
            start-placeholder="出库开始日期"
            end-placeholder="出库结束日期"
            style="width: 270px"
            class="filter-item"
            @change="crud.toQuery"
          />
        </template>
        <template #secondLineFirstItem>
          <el-tag class="filter-item" effect="plain" size="medium" type="warning">退库材料信息</el-tag>
        </template>
      </mat-header-query>

      <rrOperation />
    </div>
    <crudOperation>
      <template #optLeft>
        <span class="tip">* 可在上方填写需要退库的材料信息，将匹配与该材料信息相符的退库列表</span>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { ref, inject } from 'vue'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'
import projectCascader from '@comp-base/project-cascader.vue'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])
const isComponent = inject('isComponent')
const basicClass = inject('basicClass')

const defaultQuery = {
  createTime: [], // [借用开始日期，借用结束日期]
  // projectId: { value: undefined, resetAble: false }, // 原项目id
  basicClass: { value: basicClass, resetAble: false }
}

const { crud, query } = regHeader(defaultQuery)

// 基础类型发生变化
async function handleBasicClassChange(val) {
  await crud.resetQuery()
  crud.data = []
  crud.setColumns()
}
</script>

<style lang="scss" scoped>
.tip {
  color: #67c23a;
}
</style>
