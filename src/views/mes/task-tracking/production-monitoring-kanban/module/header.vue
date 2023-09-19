<template>
  <div class="head-container">
    <workshop-select
      ref="workshopInfRef"
      v-model="query.workshopId"
      placeholder="请选择车间"
      :factory-id="query.factoryId"
      :workshop-type="workshopTypeEnum.BUILDING.V"
      style="width: 200px"
      class="filter-item"
      clearable
      default
      defaultValue
      @change="crud.toQuery"
    />
    <el-row v-permission="permission.statistics" v-loading="projectInfo.loading" :gutter="24" class="panel-group">
      <el-col :span="5" class="card-panel-col">
        <panel name="设计产能（吨）:" :precision="2" num-color="#1890ff" :end-val="projectInfo.summary.capacityNetWeight / 1000 || 0" />
      </el-col>
      <el-col :span="6" class="card-panel-col">
        <panel
          name="年度累计和平均（吨）:"
          num-color="#1890ff"
          :num-arr="projectInfo.summary?.totalWeight"
          is-array
        />
      </el-col>
      <!-- <el-col :span="5">
          <el-statistic :value="`${projectInfo.summary?.taskNetWeight}`">
            <template #title>
              <div style="display: inline-flex; align-items: center">年度累计和平均（吨）:</div>
            </template>
            <template #suffix>/{{ projectInfo.summary?.yearAvgNetWeight }}</template>
          </el-statistic>
        </el-col> -->
      <el-col :span="4" class="card-panel-col">
        <panel name="当前在手订单（个）:" num-color="#1890ff" :end-val="projectInfo.summary?.orderQuantity || 0" />
      </el-col>
      <el-col :span="5" class="card-panel-col">
        <panel name="在手未完成量（吨）:" :precision="2" num-color="#1890ff" :end-val="projectInfo.summary.unNetWeight / 1000 || 0" />
      </el-col>
      <el-col :span="4" class="card-panel-col">
        <panel name="产能空余率（%）:" :precision="2" num-color="#1890ff" :end-val="projectInfo.summary.capacityRatio || 0" />
      </el-col>
    </el-row>
    <crudOperation>
      <template #optLeft>
        <common-radio-button
          v-model="query.status"
          :options="purchaseOrderStatusEnum.ENUM"
          class="filter-item"
          showOptionAll
          type="enum"
          @change="crud.toQuery"
        />
        <el-input
          v-model.trim="query.name"
          placeholder="输入项目搜索"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
          @keyup.enter="crud.toQuery"
        />
        <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
        <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
          重置
        </common-button>
      </template>
      <template #viewLeft>
        <slot name="btn"></slot>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { inject } from 'vue'
import { regHeader } from '@compos/use-crud'
import { purchaseOrderStatusEnum } from '@enum-ms/contract'
import { workshopTypeEnum } from '@enum-ms/common'
import workshopSelect from '@comp-mes/workshop-select'
import Panel from './panel.vue'
import crudOperation from '@crud/CRUD.operation'
// import { ElStatistic } from 'element-plus'

const projectInfo = inject('projectInfo')
const permission = inject('permission')
const defaultQuery = {
  workshopId: undefined
}

const { crud, query } = regHeader(defaultQuery)

function searchQuery() {
  crud.toQuery()
}

function resetQuery() {
  query.status = undefined
  query.name = undefined
  crud.toQuery()
}
</script>

<style>
</style>
