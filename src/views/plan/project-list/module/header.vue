<template>
  <div>
    <el-row v-loading="projectInfo.loading" v-permission="crud.permission.statistics" :gutter="20" class="panel-group">
      <el-col :span="6" class="card-panel-col">
        <panel name="全部项目" num-color="#1890ff" :end-val="projectInfo.summary.quantity || 0" />
      </el-col>
      <el-col :span="6" class="card-panel-col">
       <panel name="进行中" num-color="#1890ff" :end-val="projectInfo.summary.processingQuantity || 0" />
      </el-col>
      <el-col :span="6" class="card-panel-col">
        <panel name="完工" num-color="#1890ff" :end-val="projectInfo.summary.completedQuantity || 0" />
      </el-col>
      <el-col :span="6" class="card-panel-col">
        <panel name="本月新增" num-color="#1890ff" :end-val="projectInfo.summary.monthNewQuantity || 0" />
      </el-col>
    </el-row>
    <crudOperation>
      <template #optRight>
         <div v-show="crud.searchToggle">
          <el-date-picker
            v-model="query.year"
            type="year"
            size="small"
            style="width: 300px"
            placeholder="选择年"
            value-format="YYYY"
            class="filter-item"
            format="YYYY"
            :disabled-date="disabledDate"
            @change="crud.toQuery"
          />
          <el-input
            v-model="query.noOrName"
            size="small"
            placeholder="输入合同编号或项目简称"
            style="width: 200px;"
            class="filter-item"
            clearable
            @blur="crud.toQuery"
          />
            <rrOperation/>
        </div>
      </template>
      <template #viewLeft>
        <print-table
          v-permission="crud.permission.print"
          api-key="myProject"
          :params="{year: crud.query.year}"
          size="mini"
          type="warning"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { inject } from 'vue'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import { parseTime } from '@/utils/date'
import Panel from '@/components/Panel'

const projectInfo = inject('projectInfo')

const defaultQuery = {
  noOrName: undefined,
  year: parseTime(new Date(), '{y}')
}

const { crud, query } = regHeader(defaultQuery)

function disabledDate(time) {
  return time > new Date()
}
</script>

<style lang="scss" scoped>
  .panel-group {
    padding-bottom: 14px;
  }
</style>
