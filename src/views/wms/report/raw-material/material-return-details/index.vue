<template>
  <div class="report-material-outbound-details app-container">
    <!--工具栏-->
    <m-header />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      highlight-current-row
      @sort-change="crud.handleSortChange"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
        <template #default="{ row }">
          <expand-secondary-info v-if="!basicClass" :basic-class="row.basicClass" :row="row" />
        </template>
      </el-expand-table-column>
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="basicClass" spec-merge sortable fixed="left">
        <template #afterIndex>
          <el-table-column
            v-if="columns.visible('returnReceipt.returnTime')"
            key="returnReceipt.returnTime"
            :show-overflow-tooltip="true"
            prop="returnReceipt.returnTime"
            label="退库时间"
            align="center"
            width="125"
            fixed="left"
            sortable="custom"
          />
        </template>
      </material-base-info-columns>
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" fixed="left" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :basic-class="basicClass" />
      <!-- 价格信息 -->
      <template v-if="showAmount">
        <amount-info-columns :columns="columns" show-invoice-type />
      </template>
      <warehouse-info-columns :columns="columns" show-project />
      <el-table-column
        v-if="columns.visible('returnReceipt.serialNumber')"
        key="returnReceipt.serialNumber"
        :show-overflow-tooltip="true"
        prop="returnReceipt.serialNumber"
        min-width="155"
        label="退库单号"
        align="left"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['RETURN']" :receipt="row.returnReceipt" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('outboundReceipt.serialNumber')"
        key="outboundReceipt.serialNumber"
        :show-overflow-tooltip="true"
        prop="outboundReceipt.serialNumber"
        min-width="155"
        label="出库单号"
        align="left"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['OUTBOUND']" :receipt="row.outboundReceipt" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('returnReceipt.applicantName')"
        key="returnReceipt.applicantName"
        :show-overflow-tooltip="true"
        prop="returnReceipt.applicantName"
        label="申请人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('returnReceipt.reviewerName')"
        key="returnReceipt.reviewerName"
        :show-overflow-tooltip="true"
        prop="returnReceipt.reviewerName"
        label="审核人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('returnReceipt.createTime')"
        key="returnReceipt.createTime"
        :show-overflow-tooltip="true"
        prop="returnReceipt.createTime"
        label="申请时间"
        align="center"
        width="125"
      />
      <el-table-column
        v-if="columns.visible('returnReceipt.reviewTime')"
        key="returnReceipt.reviewTime"
        :show-overflow-tooltip="true"
        prop="returnReceipt.reviewTime"
        label="审核时间"
        align="center"
        width="125"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import { computed, ref } from 'vue'
import { getDetails as get } from '@/api/wms/report/raw-material/return'
import { reportRawMaterialReturnDetailsPM as permission } from '@/page-permission/wms'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import checkPermission from '@/utils/system/check-permission'
import { materialHasAmountColumns } from '@/utils/columns-format/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MHeader from './module/header.vue'

import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import AmountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const expandRowKeys = ref([])
const tableRef = ref()

// 表格列数据格式转换
const columnsDataFormat = ref([
  ...materialHasAmountColumns,
  ['returnReceipt.returnTime', 'parse-time'],
  ['returnReceipt.reviewTime', 'parse-time'],
  ['returnReceipt.createTime', 'parse-time']
])

const { CRUD, crud, columns } = useCRUD(
  {
    title: '退库明细',
    sort: ['id.desc'],
    invisibleColumns: [
      'returnReceipt.applicantName',
      'returnReceipt.reviewerName',
      'returnReceipt.createTime',
      'returnReceipt.reviewTime',
      'invoiceType',
      'taxRate',
      'unitPrice',
      'amount',
      'amountExcludingVAT',
      'inputVAT'
    ],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 是否有显示金额权限
const showAmount = computed(() => checkPermission(permission.showAmount))

const basicClass = computed(() => (crud.query ? crud.query.basicClass : undefined))

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content)
  data.content.forEach((row) => {
    if (!row.outboundReceipt) row.outboundReceipt = {}
    if (!row.returnReceipt) row.returnReceipt = {}
  })
}
</script>

<style lang="scss" scoped>
.report-material-outbound-details {
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
}
</style>
