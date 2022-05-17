<template>
  <div class="head-container">
      <el-row  v-loading="projectInfo.loading" v-permission="crud.permission.statistics" :gutter="20" class="panel-group">
      <!-- <el-row :gutter="20" class="panel-group"> -->
      <el-col :span="6" class="card-panel-col">
        <panel name="套料项目" num-color="#1890ff" :end-val="projectInfo.summary.projectNum || 0" />
      </el-col>
      <!-- <el-col :span="6" class="card-panel-col">
        <panel name="套料项目"   num-color="#1890ff" />
      </el-col> -->
      <el-col :span="6" class="card-panel-col">
       <panel name="全部套料" num-color="#1890ff" :end-val="projectInfo.summary.nestingNum || 0" />
      </el-col>
      <!-- <el-col :span="6" class="card-panel-col">
       <panel name="全部套料"  num-color="#1890ff" />
      </el-col> -->
      <!-- <el-col :span="6" class="card-panel-col">
        <panel name="部分套料"  num-color="#1890ff" />
      </el-col> -->
      <el-col :span="6" class="card-panel-col">
        <panel name="部分套料" num-color="#1890ff" :end-val="projectInfo.summary.partialNesting || 0" />
      </el-col>
      <!-- <el-col :span="6" class="card-panel-col">
        <panel name="未套料"  num-color="#1890ff" />
      </el-col> -->
      <el-col :span="6" class="card-panel-col">
        <panel name="未套料" num-color="#1890ff" :end-val="projectInfo.summary.noNesting || 0" />
      </el-col>
    </el-row>
    <crudOperation>
      <template #optLeft>
        <div v-show="crud.searchToggle">
             <el-date-picker
            v-model="query.year"
            type="year"
            size="small"
            class="date-item filter-item"
            style="width: 100px !important"
            format="YYYY"
            value-format="YYYY"
            placeholder="选择年"
            :disabled-date="disabledDate"
            @change="crud.toQuery"
          />
          <common-radio-button
          style="margin-right: 8px"
          class="filter-item"
          v-model="query.nestingState"
          :options="nestingStateEnum.ENUM"
          show-option-all
          type="enum"
          size="small"
          @change="crud.toQuery"
        />
          <el-input
            v-model="query.projectName"
            placeholder="请输入项目名称"
            class="filter-item"
            style="width: 200px"
            size="small"
            clearable
            @keyup.enter="crud.toQuery"
          />
          <rrOperation />
        </div>
      </template>
    </crudOperation>

  </div>
</template>

<script setup>
import { inject } from 'vue'

import { regHeader } from '@compos/use-crud'
import { nestingStateEnum } from '@enum-ms/cutting'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { parseTime } from '@/utils/date'
import Panel from '@/components/Panel'

const projectInfo = inject('projectInfo')

const defaultQuery = {
  projectName: undefined,
  year: parseTime(new Date(), '{y}')
  // monomerValue: undefined,
  // areaValue: undefined
}

const { crud, query } = regHeader(defaultQuery)

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}
</script>

<style lang="scss" scoped>
  .panel-group {
    padding-bottom: 14px;
  }
</style>

