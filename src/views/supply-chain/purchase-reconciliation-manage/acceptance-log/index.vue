<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      highlight-current-row
      row-key="id"
      show-summary
      :summary-method="getSummaries"
    >
      <!-- 基础信息 -->
      <material-base-info-columns />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns />
      <el-table-column
        v-if="columns.visible('budgetUnitPrice')"
        show-overflow-tooltip
        key="budgetUnitPrice"
        prop="budgetUnitPrice"
        label="预算单价（不含税）"
        min-width="130"
        align="right"
      />
      <el-table-column
        v-if="columns.visible('unitPriceExcludingVAT')"
        show-overflow-tooltip
        key="unitPriceExcludingVAT"
        prop="unitPriceExcludingVAT"
        label="采购单价（不含税）"
        min-width="130"
        align="right"
      />
      <el-table-column
        v-if="columns.visible('amountExcludingVAT')"
        show-overflow-tooltip
        key="amountExcludingVAT"
        prop="amountExcludingVAT"
        label="金额"
        min-width="130"
        align="right"
      />
      <el-table-column
        v-if="columns.visible('inboundTime')"
        show-overflow-tooltip
        key="inboundTime"
        prop="inboundTime"
        label="入库时间"
        width="150"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('remark')"
        show-overflow-tooltip
        key="remark"
        prop="remark"
        label="备注"
        min-width="150"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/purchase-reconciliation-manage/acceptance-log'
import { ref } from 'vue'

import { purchaseAcceptanceLogPM as permission } from '@/page-permission/supply-chain'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { tableSummary } from '@/utils/el-extra'
import { materialHasAmountColumns } from '@/utils/columns-format/wms'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([
  ...materialHasAmountColumns,
  ['inboundTime', 'parse-time']
])

const { crud, columns, CRUD } = useCRUD(
  {
    title: '验收记录',
    sort: [],
    permission: { ...permission },
    requiredQuery: ['purchaseOrderId'],
    crudApi: { ...crudApi },
    invisibleColumns: [],
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity', 'mete', 'amountExcludingVAT'],
    toThousandFields: ['amountExcludingVAT']
  })
}

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content = await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content)
}
</script>
