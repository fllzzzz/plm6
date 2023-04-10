<template>
  <div class="head-container">
    <workshop-select
      ref="workshopInfRef"
      v-model="query.workshopId"
      placeholder="请选择车间"
      :factory-id="query.factoryId"
      style="width: 200px"
      class="filter-item"
      clearable
      default
      defaultValue
      @change="crud.toQuery"
    />
    <div>
      <el-row v-permission="permission.statistics" v-loading="projectInfo.loading" :gutter="24" class="panel-group">
        <el-col :span="5" class="card-panel-col">
          <panel name="设计产能（吨）:" :decimals="2" num-color="#1890ff" :end-val="projectInfo.summary.capacityNetWeight / 1000 || 0" />
        </el-col>
        <el-col :span="5" class="card-panel-col">
          <panel name="平均产量（吨/年）:" :decimals="2" num-color="#1890ff" :end-val="projectInfo.summary.yearNetWeight / 1000 || 0" />
        </el-col>
        <el-col :span="4" class="card-panel-col">
          <panel name="上月产量（吨）:" :decimals="2" num-color="#1890ff" :end-val="projectInfo.summary.lastMonthNetWeight / 1000 || 0" />
        </el-col>
        <el-col :span="5" class="card-panel-col">
          <panel name="在手订单数（个）:" :decimals="2" num-color="#1890ff" :end-val="projectInfo.summary.orderQuantity || 0" />
        </el-col>
        <el-col :span="5" class="card-panel-col">
          <panel name="未完成量（吨）:" :decimals="2" num-color="#1890ff" :end-val="projectInfo.summary.unNetWeight / 1000 || 0" />
        </el-col>
      </el-row>
    </div>
  </div>
</template>

<script setup>
import { inject } from 'vue'
import { regHeader } from '@compos/use-crud'
import workshopSelect from '@comp-mes/workshop-select'
import Panel from '@/components/Panel'

const projectInfo = inject('projectInfo')
const permission = inject('permission')
const defaultQuery = {
  workshopId: undefined
}

const { crud, query } = regHeader(defaultQuery)
</script>

<style>
</style>
