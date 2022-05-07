<template>
  <div class="report-material-transfer-details app-container">
    <!--工具栏-->
    <mHeader />
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
          <p>来源：<source-text-info :transfer-receipt="row.transferReceipt" class="wth-auto" /></p>
          <p>目的：<direction-text-info :transfer-receipt="row.transferReceipt" class="wth-auto" /></p>
        </template>
      </el-expand-table-column>
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="basicClass" spec-merge sortable fixed="left">
        <template #afterIndex>
          <el-table-column
            v-if="columns.visible('transferReceipt.transferTime')"
            key="transferReceipt.transferTime"
            :show-overflow-tooltip="true"
            prop="transferReceipt.transferTime"
            label="调拨时间"
            align="center"
            width="125"
            fixed="left"
            sortable="custom"
          />
          <el-table-column
            v-if="columns.visible('transferReceipt.transferType')"
            show-overflow-tooltip
            key="transferReceipt.transferType"
            prop="transferReceipt.transferType"
            label="调拨类型"
            width="90"
            align="center"
            fixed="left"
          />
          <el-table-column
            v-if="columns.visible('sourceInfo')"
            show-overflow-tooltip
            key="sourceInfo"
            prop="sourceInfo"
            label="来源"
            min-width="170"
            fixed="left"
          >
            <template #default="{ row }">
              <source-text-info :transfer-receipt="row.transferReceipt" class="ellipsis-text" />
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('directionInfo')"
            show-overflow-tooltip
            key="directionInfo"
            prop="directionInfo"
            label="目的"
            min-width="170"
            fixed="left"
          >
            <template #default="{ row }">
              <direction-text-info :transfer-receipt="row.transferReceipt" class="ellipsis-text" />
            </template>
          </el-table-column>
        </template>
      </material-base-info-columns>
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" fixed="left" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :basic-class="basicClass" />
      <!-- 价格信息 -->
      <template v-if="showAmount">
        <amount-info-columns :columns="columns" show-invoice-type/>
      </template>
      <el-table-column
        v-if="columns.visible('transferReceipt.serialNumber')"
        key="transferReceipt.serialNumber"
        :show-overflow-tooltip="true"
        prop="transferReceipt.serialNumber"
        min-width="155"
        label="调拨单号"
        align="left"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['TRANSFER']" :receipt="row.transferReceipt" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('transferReceipt.applicantName')"
        key="transferReceipt.applicantName"
        :show-overflow-tooltip="true"
        prop="transferReceipt.applicantName"
        label="申请人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('transferReceipt.reviewerName')"
        key="transferReceipt.reviewerName"
        :show-overflow-tooltip="true"
        prop="transferReceipt.reviewerName"
        label="审核人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('transferReceipt.createTime')"
        key="transferReceipt.createTime"
        :show-overflow-tooltip="true"
        prop="transferReceipt.createTime"
        label="申请时间"
        align="center"
        width="125"
      />
      <el-table-column
        v-if="columns.visible('transferReceipt.reviewTime')"
        key="transferReceipt.reviewTime"
        :show-overflow-tooltip="true"
        prop="transferReceipt.reviewTime"
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
import { getDetails as get } from '@/api/wms/report/raw-material/transfer'
import { reportRawMaterialTransferDetailsPM as permission } from '@/page-permission/wms'
import { transferTypeEnum } from '@/utils/enum/modules/wms'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { materialHasAmountColumns } from '@/utils/columns-format/wms'
import { isNotBlank } from '@/utils/data-type'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'

import SourceTextInfo from '@/views/wms/material-transfer/raw-material/review/module/source-text-info.vue'
import DirectionTextInfo from '@/views/wms/material-transfer/raw-material/review/module/direction-text-info.vue'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import AmountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
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
  ['transferReceipt.transferType', ['parse-enum', transferTypeEnum]],
  ['transferReceipt.transferTime', 'parse-time'],
  ['transferReceipt.reviewTime', 'parse-time'],
  ['transferReceipt.createTime', 'parse-time'],
  ['transferReceipt.borrowProject', ['parse-project', { onlyShortName: true }]],
  ['transferReceipt.[source].project', ['parse-project', { onlyShortName: true }]],
  ['transferReceipt.direction.project', ['parse-project', { onlyShortName: true }]]
])

const { CRUD, crud, columns } = useCRUD(
  {
    title: '调拨明细',
    sort: ['id.desc'],
    invisibleColumns: [
      'transferReceipt.applicantName',
      'transferReceipt.reviewerName',
      'transferReceipt.createTime',
      'transferReceipt.reviewTime',
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

// 基础类型
const basicClass = computed(() => (crud.query ? crud.query.basicClass : undefined))

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content)
  data.content.forEach((row) => {
    if (!row.transferReceipt) row.transferReceipt = {}
    row.projects = []
    if (isNotBlank(row.transferReceipt)) {
      if (Array.isArray(row.transferReceipt.source)) {
        row.transferReceipt.source.forEach((source) => {
          if (source.project) row.projects.push(source.project)
        })
      } else if (typeof row.transferReceipt.source === 'object') {
        if (row.transferReceipt.source.project) row.projects.push(row.transferReceipt.source.project)
        row.transferReceipt.source = [row.transferReceipt.source]
      }
      if (isNotBlank(row.transferReceipt.borrowProject)) {
        row.projects.push(row.transferReceipt.borrowProject)
      }
      if (row.transferReceipt.direction && isNotBlank(row.transferReceipt.direction.project)) {
        row.projects.push(row.transferReceipt.direction.project)
      }
    }
  })
}
</script>

<style lang="scss" scoped>
.report-material-transfer-details {
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
    .ellipsis-text {
      width: 100%;
      display: inline-block;
    }
  }
}
</style>
