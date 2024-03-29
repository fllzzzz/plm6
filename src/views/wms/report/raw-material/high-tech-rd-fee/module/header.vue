<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <el-date-picker
        v-model="query.outboundTime"
        :default-time="defaultTime"
        type="month"
        range-separator=":"
        size="small"
        value-format="x"
        unlink-panels
        :clearable="false"
        placeholder="查询月份"
        style="width: 135px"
        class="filter-item"
        @change="crud.toQuery"
      />
      <common-radio-button
        v-model="query.basicClass"
        :options="rawMatClsEnum.ENUM"
        show-option-all
        type="enum"
        size="small"
        class="filter-item"
        @change="crud.toQuery"
      />
      <material-cascader
        v-model="query.classifyId"
        :basic-class="query.basicClass"
        separator=" > "
        check-strictly
        show-all-levels
        clearable
        size="small"
        class="filter-item"
        style="width: 300px"
        placeholder="可选择/输入科目、编号搜索"
        @change="crud.toQuery"
      />
      <br />
      <warehouse-project-cascader
        v-model:projectId="query.projectId"
        v-model:projectWarehouseType="query.projectWarehouseType"
        class="filter-item"
        @change="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <!-- 打印 -->
      <template #optLeft>
        <common-button type="primary" size="mini" @click="rdFeeConfVisible = true">查看/设置高新研发费占比</common-button>
        <export-button v-permission="permission.get" :params="query" :fn="exportExcel" response-header-result>
          下载高新研发费报表（根据查询条件）
        </export-button>
      </template>
      <template #viewLeft>
        <div class="flex-rcc child-mr-7">
          <el-tag type="success" effect="plain">高新研发费 - 当月： <span v-thousand="rdFeeForMonth" v-empty /></el-tag>
          <el-tag type="success" effect="plain">高新研发费 - 当年： <span v-thousand="rdFeeForYear" v-empty /> </el-tag>
        </div>
      </template>
    </crudOperation>
    <common-drawer v-model="rdFeeConfVisible" title="高新研发费用占比列表" size="90%">
      <template #content>
        <high-tech-rd-fee-conf-view class="high-tech-rd-fee-conf-view" @save-success="crud.refresh"/>
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { exportExcel } from '@/api/wms/report/raw-material/high-tech-rd-fee'
import { inject, ref } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation.vue'

import MaterialCascader from '@comp-cls/material-cascader/index.vue'
import warehouseProjectCascader from '@comp-wms/warehouse-project-cascader'
import ExportButton from '@comp-common/export-button/index.vue'
import highTechRdFeeConfView from '@/views/config-manage/wms/high-tech-rd-fee/index.vue'

const defaultQuery = {
  outboundTime: { value: `${new Date().getTime()}`, resetAble: false }, // 月份
  projectId: undefined, // 原项目id
  factoryId: undefined, // 工厂id
  projectWarehouseType: undefined, // 仓库类型
  basicClass: undefined // 基础类型
}

const permission = inject('permission')
const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0)])

// 高新研发费总额
const rdFeeForMonth = ref()
const rdFeeForYear = ref()
const rdFeeConfVisible = ref(false)

const { CRUD, crud, query } = regHeader(defaultQuery)

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  rdFeeForMonth.value = data.rdFeeForMonth
  rdFeeForYear.value = data.rdFeeForYear
}
</script>

<style lang="scss" scoped>
.high-tech-rd-fee-conf-view {
  padding: 0;
}
</style>
