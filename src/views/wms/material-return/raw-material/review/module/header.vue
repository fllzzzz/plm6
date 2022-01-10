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
      <common-radio-button
        type="enum"
        v-model="query.reviewStatus"
        :options="reviewStatusEnum.ENUM"
        show-option-all
        clearable
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.createTime"
        :default-time="defaultTime"
        type="daterange"
        range-separator=":"
        size="small"
        value-format="x"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        unlink-panels
        start-placeholder="开始日期"
        end-placeholder="结束日期"
        style="width: 240px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-input
        v-model.trim="query.serialNumber"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按退库单号搜索"
        class="filter-item"
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model.trim="query.operatorName"
        clearable
        style="width: 200px"
        size="small"
        placeholder="申请人/编辑人/审核人"
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
import { ref, onMounted } from 'vue'
import { useRoute } from 'vue-router'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { reviewStatusEnum } from '@enum-ms/common'
import { rawMatClsEnum } from '@enum-ms/classification'

import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  createTime: [], // [开始日期，结束日期]
  basicClass: undefined, // 采购类型
  reviewStatus: undefined, // 审核状态
  projectId: { value: undefined, resetAble: false }, // 项目id
  serialNumber: undefined, // 退库单号
  operatorName: undefined // 创建人
}

const route = useRoute()
const { crud, query } = regHeader(defaultQuery)
useGlobalProjectIdChangeToQuery(crud)
onMounted(() => {
  query.basicClass = route.params.basicClass
})
</script>
