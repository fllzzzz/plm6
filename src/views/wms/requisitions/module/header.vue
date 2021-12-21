<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <common-radio-button
        v-model="query.purchaseStatus"
        :options="purchaseStatusEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-radio-group v-model="query.isProjectPreparation" class="filter-item" size="small" @change="crud.toQuery">
        <el-radio-button :label="false">公共备料</el-radio-button>
        <el-radio-button :label="true">项目备料</el-radio-button>
      </el-radio-group>
      <!-- <transition name="el-fade-in-linear">
        <project-select
          v-show="query.isProjectPreparation"
          v-model:value="projectId"
          style="width:400px;"
          class="filter-item"
          :initial="false"
          clearable
          @change="crud.toQuery"
        />
      </transition> -->
      <common-select
        v-model="query.basicClassesArr"
        :options="matClsEnum.ENUM"
        multiple
        type="enum"
        clearable
        placeholder="可选择物料基础类型搜索"
        style="width: 200px;"
        class="filter-item"
        @change="crud.toQuery"
      />
      <br>
      <el-input
        v-model="query.purchaseNo"
        placeholder="输入备料单号搜索"
        class="filter-item"
        style="width: 220px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
      />
      <el-input
        v-model="query.writtenByName"
        placeholder="输入填写人搜索"
        class="filter-item"
        style="width: 220px;"
        size="small"
        clearable
        @keyup.enter="crud.toQuery"
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
      <rrOperation />
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { purchaseStatusEnum } from '@enum-ms/wms'
import { matClsEnum } from '@enum-ms/classification'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'

import { ElRadioGroup } from 'element-plus'
import { regHeader } from '@compos/use-crud'
import useGlobalProjectIdChangeToQuery from '@compos/use-global-project-id-change-to-query'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  isProjectPreparation: true,
  status: purchaseStatusEnum.UNFINISHED.V,
  basicClasses: 0, basicClassesArr: undefined, name: undefined,
  createTime: undefined, startDate: undefined, endDate: undefined
}

const { crud, query } = regHeader(defaultQuery)
useGlobalProjectIdChangeToQuery(crud)
</script>
