<template>
  <div class="report-material-outbound-details app-container">
    <!--工具栏-->
    <m-header />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      @sort-change="crud.handleSortChange"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
        <template #default="{ row }">
          <expand-secondary-info v-if="!basicClass" :basic-class="row.basicClass" :row="row" />
          <p>关联项目：<span v-parse-project="{ project: row.projects }" v-empty-text /></p>
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
          >
            <template #default="{ row }">
              <span v-parse-time="row.returnReceipt.returnTime" />
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
          <clickable-permission-span
            v-if="row.returnReceipt"
            :permission="permission.returnReceiptDetail"
            @click="openReturnReceiptDetail(row.returnReceipt.id)"
            :text="row.returnReceipt.serialNumber"
          />
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
          <clickable-permission-span
            v-if="row.outboundReceipt"
            :permission="permission.outboundReceiptDetail"
            @click="openOutboundReceiptDetail(row.outboundReceipt.id)"
            :text="row.outboundReceipt.serialNumber"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('returnReceipt.founderName')"
        key="returnReceipt.founderName"
        :show-overflow-tooltip="true"
        prop="returnReceipt.founderName"
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
      >
        <template #default="{ row }">
          <span v-parse-time="row.returnReceipt.reviewTime" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('returnReceipt.reviewTime')"
        key="returnReceipt.reviewTime"
        :show-overflow-tooltip="true"
        prop="returnReceipt.reviewTime"
        label="审核时间"
        align="center"
        width="125"
      >
        <template #default="{ row }">
          <span v-parse-time="row.returnReceipt.reviewTime" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 出库单详情 -->
    <detail-wrapper ref="outboundReceiptDetailRef" :api="getOutboundReceiptDetail">
      <outbound-receipt-detail />
    </detail-wrapper>
    <!-- 退库单详情 -->
    <detail-wrapper ref="returnReceiptDetailRef" :api="getReturnReceiptDetail">
      <return-receipt-detail />
    </detail-wrapper>
  </div>
</template>

<script setup>
import { computed, ref } from 'vue'
import { getDetails as get } from '@/api/wms/report/raw-material/return'
import { detail as getOutboundReceiptDetail } from '@/api/wms/material-outbound/raw-material/record'
import { detail as getReturnReceiptDetail } from '@/api/wms/material-return/raw-material/record'
import { reportRawMaterialReturnDetailsPM as permission } from '@/page-permission/wms'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useOtherCrudDetail from '@/composables/use-other-crud-detail'

import DetailWrapper from '@crud/detail-wrapper.vue'
import Pagination from '@crud/Pagination'
import MHeader from './module/header.vue'

import OutboundReceiptDetail from '@/views/wms/material-outbound/raw-material/record/module/detail.vue'
import ReturnReceiptDetail from '@/views/wms/material-return/raw-material/record/module/detail.vue'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import AmountInfoColumns from '@/components-system/wms/table-columns/amount-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import ClickablePermissionSpan from '@/components-system/common/clickable-permission-span.vue'

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
    title: '退库明细',
    sort: ['id.desc'],
    invisibleColumns: [
      'returnReceipt.founderName',
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

// 是否有权限显示金额
const showAmount = computed(() => checkPermission(permission.showAmount))

const basicClass = computed(() => (crud.query ? crud.query.basicClass : undefined))

// 出库单详情
const { detailRef: outboundReceiptDetailRef, openDetail: openOutboundReceiptDetail } = useOtherCrudDetail()

// 退库单详情
const { detailRef: returnReceiptDetailRef, openDetail: openReturnReceiptDetail } = useOtherCrudDetail()

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
