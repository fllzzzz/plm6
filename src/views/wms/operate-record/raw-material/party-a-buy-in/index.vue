<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      :key="`return_to_party_a_${crud.query.basicClass}`"
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
      <material-base-info-columns :columns="columns" :basic-class="crud.query.basicClass" :show-party-a="false" show-classification classify-name-alias="名称" fixed="left" />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="crud.query.basicClass" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :basic-class="crud.query.basicClass" />
      <!-- 仓库信息 -->
      <warehouse-info-columns :columns="columns" show-project />
      <!-- 价格 -->
      <amount-info-columns :columns="columns" show-unit-price-e show-invoice-type show-tax-rate />
      <el-table-column
        v-if="columns.visible('transferSN')"
        key="transferSN"
        :show-overflow-tooltip="true"
        prop="transferSN"
        label="调拨单号"
        align="center"
        min-width="120"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['TRANSFER']" :receipt="row.transfer" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('applicantName')"
        key="applicantName"
        :show-overflow-tooltip="true"
        prop="applicantName"
        label="操作人"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('reviewTime')"
        key="reviewTime"
        :show-overflow-tooltip="true"
        prop="reviewTime"
        label="买入日期"
        align="center"
        width="140"
        sortable="custom"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-transfer/raw-material/party-a-buy-in'
import { operateRecordPartyABuyInPM as permission } from '@/page-permission/wms'

import { ref, computed } from 'vue'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { reviewTimeColumns, materialColumns } from '@/utils/columns-format/wms'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import amountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'
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
const columnsDataFormat = computed(() => {
  return [
    ...materialColumns,
    // 金额相关
    ['invoiceType', ['parse-enum', invoiceTypeEnum, { f: 'SL' }]],
    ['taxRate', ['suffix', '%']],
    ['unitPrice', ['to-thousand', decimalPrecision.value.wms]],
    ['unitPriceExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['amount', ['to-thousand', decimalPrecision.value.wms]],
    ['amountExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['inputVAT', ['to-thousand', decimalPrecision.value.wms]],
    ...reviewTimeColumns
  ]
})
// const columnsDataFormat = ref([...materialHasAmountColumns, ...reviewTimeColumns])

// 展开行
const expandRowKeys = ref([])
// 表格ref
const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '甲供买入',
    sort: ['reviewTime.desc'],
    invisibleColumns: ['heatNoAndBatchNo', 'project', 'warehouse', 'invoiceType', 'taxRate', 'transferSN', 'applicantName'],
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
