<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <common-radio-button
        type="enum"
        v-model="query.returnStatus"
        :options="borrowReturnStatusEnum.ENUM"
        show-option-all
        clearable
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        type="enum"
        v-model="query.basicClass"
        :options="rawMatClsEnum.ENUM"
        show-option-all
        clearable
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.transferTime"
        :default-time="defaultTime"
        type="daterange"
        range-separator=":"
        size="small"
        value-format="x"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        unlink-panels
        start-placeholder="借用开始日期"
        end-placeholder="借用结束日期"
        style="width: 270px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <br />
      <project-cascader
        v-model="query.projectId"
        placeholder="原项目"
        clearable
        @change="crud.toQuery"
        class="filter-item"
        style="width: 300px"
      />
      <project-cascader
        v-model="query.borrowProjectId"
        placeholder="借用项目"
        clearable
        @change="crud.toQuery"
        class="filter-item"
        style="width: 300px"
      />
      <el-input
        v-model.trim="query.transferSN"
        clearable
        style="width: 200px"
        size="small"
        placeholder="调拨单号"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.operatorName"
        clearable
        style="width: 200px"
        size="small"
        placeholder="调拨人"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
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
import { borrowReturnStatusEnum } from '@/utils/enum/modules/wms'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import projectCascader from '@comp-base/project-cascader.vue'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  returnStatus: borrowReturnStatusEnum.NOT_RETURNED.V, // 归还状态
  transferTime: [], // [借用开始日期，借用结束日期]
  projectId: undefined, // 原项目id
  borrowProjectId: undefined, // 借用项目id
  basicClass: { value: undefined, resetAble: false }, // 默认钢板
  transferorName: undefined // 调拨人
}

const { crud, query } = regHeader(defaultQuery)
</script>
