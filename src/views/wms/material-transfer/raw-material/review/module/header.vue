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
        start-placeholder="申请开始日期"
        end-placeholder="申请结束日期"
        style="width: 270px"
        class="filter-item"
        @change="crud.toQuery"
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
      <br />
      <common-radio-button
        type="enum"
        v-model="query.transferType"
        :options="transferTypeEnum.ENUM"
        show-option-all
        clearable
        class="filter-item"
        @change="crud.toQuery"
      />
      <project-cascader
        v-model="query.sourceProjectId"
        placeholder="原项目"
        clearable
        @change="crud.toQuery"
        class="filter-item"
        style="width: 300px"
      />
      <project-cascader
        v-model="query.directionProjectId"
        placeholder="目的项目"
        clearable
        @change="crud.toQuery"
        class="filter-item"
        style="width: 300px"
      />
      <el-input
        v-model.trim="query.serialNumber"
        clearable
        style="width: 200px"
        size="small"
        placeholder="按调拨单号搜索"
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
import { reviewStatusEnum } from '@enum-ms/common'
import { rawMatClsEnum } from '@enum-ms/classification'
import { transferTypeEnum } from '@/utils/enum/modules/wms'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  createTime: [], // [开始日期，结束日期]
  basicClass: undefined, // 采购类型
  transferType: undefined, // 调拨类型
  reviewStatus: reviewStatusEnum.UNREVIEWED.V, // 审核状态
  sourceProjectId: undefined, // 原项目id
  directionProjectId: undefined, // 目的项目id
  serialNumber: undefined, // 调拨单号
  operatorName: undefined // 创建人
}

const { crud, query } = regHeader(defaultQuery)
</script>
