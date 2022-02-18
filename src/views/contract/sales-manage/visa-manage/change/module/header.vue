<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <project-radio-button
        size="small"
        v-model="query.projectId"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.status"
        :options="visaReviewStatusEnum.ENUM"
        showOptionAll
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.year"
        placeholder="选择申请年份"
        type="year"
        format="YYYY"
        value-format="YYYY"
        size="small"
        class="filter-item date-item"
        style="width: 100px"
        @change="crud.toQuery"
      />
      <el-input
        v-model="query.userName"
        placeholder="可输入申请人搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.checkUerName"
        placeholder="可输入审核人搜索"
        class="filter-item"
        style="width: 200px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <rrOperation/>
    </div>
    <crudOperation>
      <template #optLeft>
        <common-button v-permission="crud.permission.add" type="primary" size="mini" @click.stop="toAdd(visaTypeEnum.VISA.V)">新增签证单</common-button>
        <common-button v-permission="crud.permission.add" type="danger" size="mini" @click.stop="toAdd(visaTypeEnum.SETTLEMENT.V)">新增结算单</common-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineEmits } from 'vue'

import { visaReviewStatusEnum, visaTypeEnum } from '@enum-ms/common'
import { businessTypeEnum } from '@enum-ms/contract'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const emit = defineEmits(['toAdd'])

const defaultQuery = {
  userName: undefined, checkUerName: undefined,
  year: undefined, businessType: businessTypeEnum.MACHINING.V,
  projectId: { value: undefined, resetAble: false }
}
const { crud, query } = regHeader(defaultQuery)

// 新增
function toAdd(type) {
  emit('toAdd', type)
}
</script>
