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
        type="enumSL"
        v-model="query.orderSupplyType"
        :options="orderSupplyTypeEnum.ENUM"
        show-option-all
        clearable
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
        multiple
        size="small"
        class="filter-item"
        style="width: 300px"
        placeholder="可选择/输入科目、编号搜索"
        @change="crud.toQuery"
      />
      <rrOperation />
    </div>
    <crudOperation>
      <template #optLeft>
        <export-button v-permission="permission.download" :params="query" :fn="excel" response-header-result>
          下载库存报表（根据查询条件）
        </export-button>
      </template>
      <template #viewLeft v-if="query.basicClass & STEEL_ENUM">
        <span v-permission="permission.get">
          <el-tag effect="plain" class="filter-item" size="medium">
            <span>总量：</span>
            <span v-if="!summaryLoading">
              {{ convertUnits(summaryMete, 'g', STEEL_BASE_UNIT.weight.unit, STEEL_BASE_UNIT.weight.precision) }}
              {{ STEEL_BASE_UNIT.weight.unit }}
            </span>
            <i v-else class="el-icon-loading" />
          </el-tag>
        </span>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { excel, getSummary } from '@/api/wms/report/raw-material/inventory'
import { inject, ref } from 'vue'
import { rawMatClsEnum } from '@enum-ms/classification'
import { orderSupplyTypeEnum } from '@enum-ms/wms'
import { STEEL_ENUM, STEEL_BASE_UNIT } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'

import { regHeader } from '@compos/use-crud'
import rrOperation from '@crud/RR.operation'
import crudOperation from '@crud/CRUD.operation'
import MaterialCascader from '@comp-cls/material-cascader/index.vue'
import ExportButton from '@comp-common/export-button/index.vue'

const defaultQuery = {
  basicClass: undefined, // 采购类型
  classifyId: undefined // 科目id
}

const permission = inject('permission')
const { crud, query, CRUD } = regHeader(defaultQuery)

const summaryLoading = ref(false)
const summaryMete = ref()

CRUD.HOOK.beforeToQuery = () => {
  fetchSummaryInfo()
}

async function fetchSummaryInfo() {
  if (!(query.basicClass & STEEL_ENUM)) {
    return
  }
  summaryLoading.value = true
  try {
    summaryMete.value = (await getSummary(crud.query)) || 0
  } catch (error) {
    console.log('获取汇总数据', error)
  } finally {
    summaryLoading.value = false
  }
}
</script>
