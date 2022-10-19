<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <common-radio-button
        v-model="query.status"
        :options="scheduleStatusEnum.ENUM"
        showOptionAll
        :optionAllValue="undefined"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.year"
        type="year"
        format="YYYY"
        class="filter-item"
        value-format="x"
        placeholder="请选择"
        @change="crud.toQuery"
      />
      <common-select
        v-model="query.month"
        :options="monthNumEnum.ENUM"
        clearable
        type="enum"
        size="small"
        placeholder="请选择"
        class="filter-item"
        @change="crud.toQuery"
      />
      <rrOperation />
      <el-row v-loading="crud.loading" v-if="checkPermission(crud.permission.get)" :gutter="20" class="panel-group">
        <el-col :span="6" class="card-panel-col">
          <Panel name="项目总数（个）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.projectQuantity || 0" :precision="0" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="项目总量（t）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.projectQuantityWeight || 0" :precision="DP.COM_WT__T" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="已排产（t）" text-color="#626262" num-color="#1890ff" :end-val="totalAmount.schedulingTotalNetWeight || 0" :precision="DP.COM_WT__T" />
        </el-col>
        <el-col :span="6" class="card-panel-col">
          <Panel name="未排产（t）" text-color="#626262" num-color="#f56c6c" :end-val="totalAmount.unSchedulingTotalNetWeight || 0" :precision="DP.COM_WT__T" />
        </el-col>
      </el-row>
    </div>
    <crudOperation />
  </div>
</template>

<script setup>
import { ref, watch } from 'vue'
import { regHeader } from '@compos/use-crud'
import { productionOrderSummary } from '@/api/mes/production-order-manage/production-order'

import checkPermission from '@/utils/system/check-permission'
import { scheduleStatusEnum, monthNumEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'

import Panel from '@/components/Panel'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'

const defaultQuery = {
  status: undefined,
  year: new Date().getTime(),
  month: undefined
}
const { crud, query } = regHeader(defaultQuery)

const totalAmount = ref({})

watch(
  () => crud.query,
  (val) => {
    getSummary()
  },
  { deep: true, immediate: true }
)

async function getSummary() {
  totalAmount.value = {}
  try {
    const data = await productionOrderSummary(crud.query)
    data.projectQuantityWeight = data.projectQuantityWeight ? data.projectQuantityWeight / 1000 : 0
    data.schedulingTotalNetWeight = data.schedulingTotalNetWeight ? data.schedulingTotalNetWeight / 1000 : 0
    data.unSchedulingTotalNetWeight = data.unSchedulingTotalNetWeight ? data.unSchedulingTotalNetWeight / 1000 : 0
    totalAmount.value = data || {}
  } catch (error) {
    console.log('获取生产订单汇总失败', error)
  }
}

</script>
<style lang="scss" scoped>
.panel-group {
  margin-bottom:10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        text-align:left;
        margin-top: 2px;
      }
      .card-panel-num {
        display:block;
        font-size: 20px;
        text-align:right;
      }
    }
  }
}
</style>
