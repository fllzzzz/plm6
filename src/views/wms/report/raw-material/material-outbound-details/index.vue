<template>
  <div class="report-material-outbound-details app-container">
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
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
        <template #default="{ row }">
          <expand-secondary-info v-if="!basicClass" :basic-class="row.basicClass" :row="row" />
          <p>关联项目：<span v-parse-project="{ project: row.projects }" v-empty-text /></p>
        </template>
      </el-expand-table-column>
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="basicClass" spec-merge fixed="left">
        <template #afterIndex>
          <el-table-column
            v-if="columns.visible('outboundReceipt.outboundTime')"
            key="outboundReceipt.outboundTime"
            :show-overflow-tooltip="true"
            prop="outboundReceipt.outboundTime"
            label="出库时间"
            align="center"
            width="125"
            fixed="left"
          >
            <template #default="{ row }">
              <span v-parse-time="row.outboundReceipt.outboundTime" />
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
        v-if="columns.visible('outboundReceipt.founderName')"
        key="outboundReceipt.founderName"
        :show-overflow-tooltip="true"
        prop="outboundReceipt.founderName"
        label="申请人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('outboundReceipt.reviewerName')"
        key="outboundReceipt.reviewerName"
        :show-overflow-tooltip="true"
        prop="outboundReceipt.reviewerName"
        label="审核人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('outboundReceipt.createTime')"
        key="outboundReceipt.createTime"
        :show-overflow-tooltip="true"
        prop="outboundReceipt.createTime"
        label="申请时间"
        align="center"
        width="125"
      >
        <template #default="{ row }">
          <span v-parse-time="row.outboundReceipt.createTime" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('outboundReceipt.reviewTime')"
        key="outboundReceipt.reviewTime"
        :show-overflow-tooltip="true"
        prop="outboundReceipt.reviewTime"
        label="审核时间"
        align="center"
        width="125"
      >
        <template #default="{ row }">
          <span v-parse-time="row.outboundReceipt.reviewTime" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 出库单详情 -->
    <detail-wrapper ref="outboundReceiptDetailRef" :api="getOutboundReceiptDetail">
      <outbound-receipt-detail />
    </detail-wrapper>
  </div>
</template>

<script setup>
import { computed, ref } from 'vue'
import { getDetails as get } from '@/api/wms/report/raw-material/outbound'
import { detail as getOutboundReceiptDetail } from '@/api/wms/material-outbound/raw-material/record'
import { reportRawMaterialOutboundDetailsPM as permission } from '@/page-permission/wms'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useOtherCrudDetail from '@/composables/use-other-crud-detail'

import DetailWrapper from '@crud/detail-wrapper.vue'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'

import OutboundReceiptDetail from '@/views/wms/material-outbound/raw-material/record/module/detail.vue'
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
    title: '出库明细',
    sort: ['id.desc'],
    invisibleColumns: [
      'outboundReceipt.founderName',
      'outboundReceipt.reviewerName',
      'outboundReceipt.createTime',
      'outboundReceipt.reviewTime',
      'invoiceType',
      'taxRate'
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

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content)
  data.content.forEach((row) => {
    if (!row.outboundReceipt) row.outboundReceipt = {}
  })
  // 退货信息转换
  const rejectList = []
  data.content.forEach((row) => {
    if (Array.isArray(row.rejectList)) {
      row.rejectList.forEach((rr) => {
        rejectList.push(rr.material)
      })
    }
  })
  await setSpecInfoToList(rejectList)
  await numFmtByBasicClass(rejectList)
}
</script>

<style lang="scss" scoped>
.report-material-outbound-details {
  .el-table {
    ::v-deep(.cell) {
      height: 28px;
      line-height: 28px;
    }
  }
}
</style>
