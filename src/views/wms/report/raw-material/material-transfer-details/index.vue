<template>
  <div class="report-material-transfer-details app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
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
          >
            <template #default="{ row }">
              <span v-parse-time="row.transferReceipt.transferTime" />
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('transferReceipt.transferType')"
            show-overflow-tooltip
            key="transferReceipt.transferType"
            prop="transferReceipt.transferType"
            label="调拨类型"
            width="90"
            align="center"
            fixed="left"
          >
            <template #default="{ row }">
              <span v-parse-enum="{ e: transferTypeEnum, v: row.transferReceipt.transferType }" />
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('transferReceipt.source')"
            show-overflow-tooltip
            key="transferReceipt.source"
            prop="transferReceipt.source"
            label="来源"
            min-width="170"
            fixed="left"
          >
            <template #default="{ row }">
              <source-text-info :transfer-receipt="row.transferReceipt" class="ellipsis-text" />
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('transferReceipt.direction')"
            show-overflow-tooltip
            key="transferReceipt.direction"
            prop="transferReceipt.direction"
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
        <amount-info-columns :columns="columns" show-invoice-type show-tax-rate />
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
          <clickable-permission-span
            v-if="row.transferReceipt"
            :permission="permission.transferReceiptDetail"
            @click="openTransferReceiptDetail(row.transferReceipt.id)"
            :text="row.transferReceipt.serialNumber"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('transferReceipt.founderName')"
        key="transferReceipt.founderName"
        :show-overflow-tooltip="true"
        prop="transferReceipt.founderName"
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
      >
        <template #default="{ row }">
          <span v-parse-time="row.transferReceipt.createTime" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('transferReceipt.reviewTime')"
        key="transferReceipt.reviewTime"
        :show-overflow-tooltip="true"
        prop="transferReceipt.reviewTime"
        label="审核时间"
        align="center"
        width="125"
      >
        <template #default="{ row }">
          <span v-parse-time="row.transferReceipt.reviewTime" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 调拨单详情 -->
    <detail-wrapper ref="transferReceiptDetailRef" :api="getTransferReceiptDetail">
      <transfer-receipt-detail />
    </detail-wrapper>
  </div>
</template>

<script setup>
import { computed, ref } from 'vue'
import { getDetails as get } from '@/api/wms/report/raw-material/transfer'
import { detail as getTransferReceiptDetail } from '@/api/wms/material-transfer/raw-material/review'
import { reportRawMaterialTransferDetailsPM as permission } from '@/page-permission/wms'
import { transferTypeEnum } from '@/utils/enum/modules/wms'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useOtherCrudDetail from '@/composables/use-other-crud-detail'

import DetailWrapper from '@crud/detail-wrapper.vue'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'

import SourceTextInfo from '@/views/wms/material-transfer/raw-material/review/module/source-text-info.vue'
import DirectionTextInfo from '@/views/wms/material-transfer/raw-material/review/module/direction-text-info.vue'
import TransferReceiptDetail from '@/views/wms/material-transfer/raw-material/review/module/detail.vue'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import AmountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import ClickablePermissionSpan from '@/components-system/common/clickable-permission-span.vue'
import { isNotBlank } from '@/utils/data-type'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const expandRowKeys = ref([])
const tableRef = ref()

const { CRUD, crud, columns } = useCRUD(
  {
    title: '调拨明细',
    sort: ['id.desc'],
    invisibleColumns: [
      'transferReceipt.founderName',
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

// 是否有权限显示金额
const showAmount = computed(() => checkPermission(permission.showAmount))

// 基础类型
const basicClass = computed(() => (crud.query ? crud.query.basicClass : undefined))

// 调拨单详情
const { detailRef: transferReceiptDetailRef, openDetail: openTransferReceiptDetail } = useOtherCrudDetail()

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
