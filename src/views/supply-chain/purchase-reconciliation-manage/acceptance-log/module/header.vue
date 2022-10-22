<template>
  <div class="head-container">
    <div v-show="crud.searchToggle">
      <el-tooltip content="过滤采购合同" :show-after="100" placement="right">
        <span>
          <el-date-picker
            v-model="query.year"
            type="year"
            size="small"
            class="filter-item"
            style="width:100px!important"
            :disabled-date="disabledDate"
            placeholder="选择年"
            format="YYYY"
            value-format="YYYY"
            @change="crud.toQuery"
          />
        </span>
      </el-tooltip>
      <supplier-select
        v-model="query.supplierId"
        :type="supplierTypeEnum.RAW_MATERIAL.V"
        clearable
        class="filter-item"
        placeholder="可选择供应商搜索"
        show-hide
        style="width: 300px"
        @change="crud.toQuery"
      />
      <purchase-order-select
        class="filter-item"
        v-model="query.purchaseOrderId"
        :supplier-id="query.supplierId"
        :year="query.year"
        clearable
        style="width: 300px"
        @change="crud.toQuery"
      />
      <el-date-picker
        v-model="query.inboundTime"
        :default-time="defaultTime"
        type="daterange"
        range-separator=":"
        size="small"
        class="filter-item"
        value-format="x"
        start-placeholder="入库日期"
        end-placeholder="入库日期"
        :shortcuts="PICKER_OPTIONS_SHORTCUTS"
        :disabled-date="disabledDate"
        style="width:240px"
        @change="crud.toQuery"
      />
      <rrOperation/>
    </div>
    <crudOperation>
      <template #optLeft>
        <el-tag v-loading="crud.loading" type="success" effect="plain" size="medium">总额（元）：{{ totalAmount }}</el-tag>
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
import { supplierTypeEnum } from '@enum-ms/supplier'
import { PICKER_OPTIONS_SHORTCUTS, STEEL_ENUM } from '@/settings/config'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { toThousand, getDP } from '@data-type/number'

import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import rrOperation from '@crud/RR.operation'
import ExportButton from '@comp-common/export-button/index.vue'
import purchaseOrderSelect from '@/components-system/wms/purchase-order-select/index.vue'
import supplierSelect from '@comp-base/supplier-select/index.vue'

const defaultTime = ref([new Date(2000, 1, 1, 0, 0, 0), new Date(2000, 2, 1, 23, 59, 59)])

const defaultQuery = {
  inboundTime: [], // [开始时间，结束时间]
  year: undefined,
  supplierId: undefined,
  purchaseOrderId: undefined
}

const totalAmount = ref(0)
const permission = inject('permission')
const { CRUD, crud, query } = regHeader(defaultQuery)

function disabledDate(time) {
  return time > new Date()
}

// 重新查询前
CRUD.HOOK.beforeToQuery = () => {
  if (!query.inboundTime) {
    query.inboundTime = undefined
  }
  if (!query.year) {
    query.year = undefined
  }
}

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  // 汇总金额
  totalAmount.value = toThousand(data.supplierReconciliationAmount) || 0
  data.content = await setSpecInfoToList(data.content)
  data.content.forEach(v => {
    if (v.basicClass < STEEL_ENUM) {
      // 此页面钢材默认显示吨，保留3位
      v.accountingUnit = '吨'
      v.accountingPrecision = 3
    }
  })
  data.content = await numFmtByBasicClass(data.content, { toNum: true })
  data.content.forEach(v => {
    v.unitPriceExcludingVAT = toThousand(v.unitPriceExcludingVAT, getDP(v.unitPriceExcludingVAT))
  })
}
</script>
