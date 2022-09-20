<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-date-picker
        v-model="query.year"
        type="year"
        size="small"
        class="filter-item"
        style="width:100px!important"
        :disabled-date="disabledDate"
        :clearable="false"
        placeholder="选择年"
        format="YYYY"
        value-format="YYYY"
        @change="crud.toQuery"
        />
        <purchase-order-select
          class="filter-item"
          v-model="query.purchaseOrderId"
          default
          @change="crud.toQuery"
          @info-change="getInfo"
          style="width: 300px"
        />
      <rrOperation/>
    </div>
    <crudOperation>
      <template #optLeft>
        <el-tag v-if="supplierName" size="medium">供应商：{{ supplierName }}</el-tag>
      </template>
      <template #viewLeft>
        <export-button v-permission="permission.download" :params="query" :fn="excel" response-header-result>
          下载验收记录（根据查询条件）
        </export-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { excel } from '@/api/supply-chain/purchase-reconciliation-manage/acceptance-log'
import { ref, inject } from 'vue'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import ExportButton from '@comp-common/export-button/index.vue'
import purchaseOrderSelect from '@/components-system/wms/purchase-order-select/index.vue'

const defaultQuery = {
  purchaseOrderId: undefined,
  year: String(new Date().getFullYear())
}

const supplierName = ref('')
const permission = inject('permission')
const { crud, query } = regHeader(defaultQuery)

function disabledDate(time) {
  return time > new Date()
}

// 获取采购订单信息
function getInfo(row) {
  supplierName.value = row?.supplier?.name
}
</script>
