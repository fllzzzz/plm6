<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      @sort-change="crud.handleSortChange"
    >
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="crud.query.basicClass" show-classification classify-name-alias="名称" show-project spec-merge>
        <template #afterIndex>
          <el-table-column
            v-if="columns.visible('outboundReceipt.outboundTime')"
            key="outboundReceipt.outboundTime"
            show-overflow-tooltip
            prop="outboundReceipt.outboundTime"
            label="出库日期"
            align="center"
            width="120"
            sortable="custom"
          />
        </template>
      </material-base-info-columns>
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :basic-class="crud.query.basicClass" show-steel-unit />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="crud.query.basicClass" show-batch-no />
      <!-- 金额信息 -->
      <amount-info-columns :columns="columns" show-unit-price-e show-invoice-type show-tax-rate :show-input-VAT="false" />
      <!-- 仓库信息 -->
      <warehouse-info-columns :columns="columns" />
      <!-- 研发费用占比 及 研发费-->
      <el-table-column
        v-if="columns.visible('rdRate')"
        key="rdRate"
        show-overflow-tooltip
        prop="rdRate"
        label="研发费占比"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('rdFee')"
        key="rdFee"
        show-overflow-tooltip
        prop="rdFee"
        label="研发费"
        align="right"
        width="135"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/report/raw-material/high-tech-rd-fee'
import { reportRawMaterialHighTechRDFeePM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { materialHasAmountColumns } from '@/utils/columns-format/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import AmountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

// 表格列数据格式转换
const columnsDataFormat = ref([
  ...materialHasAmountColumns,
  ['outboundReceipt.outboundTime', ['parse-time', '{y}-{m}-{d}']],
  ['rdRate', ['suffix', ' %']],
  ['rdFee', ['to-thousand', decimalPrecision.wms]]
])

// 展开行
const expandRowKeys = ref([])
// 表格ref
const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '高薪研发费',
    sort: [],
    invisibleColumns: ['quantity', 'measureUnit', 'brand', 'warehouse', 'invoiceType', 'taxRate', 'unitPriceExcludingVAT'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: false
  })
}
</script>
