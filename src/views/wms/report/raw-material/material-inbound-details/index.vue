<template>
  <div class="report-material-inbound-details app-container">
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
      <material-base-info-columns
        :columns="columns"
        :basic-class="basicClass"
        show-reject-status
        reject-detail-viewable
        spec-merge
        fixed="left"
      >
        <template #afterIndex>
          <el-table-column
            v-if="columns.visible('inboundReceipt.reviewTime')"
            key="inboundReceipt.reviewTime"
            :show-overflow-tooltip="true"
            prop="inboundReceipt.reviewTime"
            label="入库时间"
            align="center"
            width="125"
            fixed="left"
          >
            <template #default="{ row }">
              <span v-parse-time>{{ row.inboundReceipt.reviewTime }}</span>
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
        <amount-info-columns :columns="columns" />
      </template>
      <warehouse-info-columns :columns="columns" show-project />
      <el-table-column
        v-if="columns.visible('inboundReceipt.purchaseOrder')"
        key="inboundReceipt.purchaseOrder"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.purchaseOrder"
        label="采购单号"
        min-width="155"
      >
        <template #default="{ row }">
          <clickable-permission-span
            v-if="row.inboundReceipt.purchaseOrder"
            :permission="permission.purchaseOrderDetail"
            @click="openPurchaseOrderDetail(row.inboundReceipt.purchaseOrder.id)"
            :text="row.inboundReceipt.purchaseOrder.serialNumber"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inboundReceipt.serialNumber')"
        key="inboundReceipt.serialNumber"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.serialNumber"
        min-width="155"
        label="入库单号"
        align="left"
      >
        <template #default="{ row }">
          <clickable-permission-span
            v-if="row.inboundReceipt"
            :permission="permission.inboundDetail"
            @click="openInboundReceiptDetail(row.inboundReceipt.id)"
            :text="row.inboundReceipt.serialNumber"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inboundReceipt.licensePlate')"
        key="inboundReceipt.licensePlate"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.licensePlate"
        label="车牌号"
        align="left"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('inboundReceipt.shipmentNumber')"
        key="inboundReceipt.shipmentNumber"
        prop="inboundReceipt.shipmentNumber"
        label="物流单号"
        align="left"
        min-width="150"
        show-overflow-tooltip
      />
      <el-table-column
        v-if="columns.visible('inboundReceipt.supplier.name')"
        key="inboundReceipt.supplier.name"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.supplier.name"
        label="供应商"
        min-width="200"
      />
      <el-table-column
        v-if="columns.visible('inboundReceipt.founderName')"
        key="inboundReceipt.founderName"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.founderName"
        label="申请人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('inboundReceipt.editorName')"
        key="inboundReceipt.editorName"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.editorName"
        label="编辑人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('inboundReceipt.reviewerName')"
        key="inboundReceipt.reviewerName"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.reviewerName"
        label="审核人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('inboundReceipt.createTime')"
        key="inboundReceipt.createTime"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.createTime"
        label="申请时间"
        align="center"
        width="125"
      >
        <template #default="{ row }">
          <span v-parse-time>{{ row.inboundReceipt.createTime }}</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 查看详情 -->
    <m-detail />
    <!-- 采购订单详情 -->
    <detail-wrapper ref="purchaseOrderRef" :api="getPurchaseOrderDetail">
      <purchase-order-detail />
    </detail-wrapper>
    <!-- 入库单详情 -->
    <detail-wrapper ref="inboundReceiptDetailRef" :api="getInboundReceiptDetail">
      <inbound-receipt-detail />
    </detail-wrapper>
  </div>
</template>

<script setup>
import { computed, ref } from 'vue'
import { getDetails as get } from '@/api/wms/report/raw-material/inbound'
import { detail as getPurchaseOrderDetail } from '@/api/wms/purchase-order'
import { detail as getInboundReceiptDetail } from '@/api/wms/material-inbound/raw-material/review'
import { reportRawMaterialInboundDetailsPM as permission } from '@/page-permission/wms'
import checkPermission from '@/utils/system/check-permission'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useOtherCrudDetail from '@/composables/use-other-crud-detail'

import DetailWrapper from '@crud/detail-wrapper.vue'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'
import MDetail from './module/detail.vue'

import InboundReceiptDetail from '@/views/wms/material-inbound/raw-material/review/module/detail.vue'
import PurchaseOrderDetail from '@/views/wms/purchase-order/module/detail.vue'
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
    title: '入库明细',
    sort: ['id.desc'],
    invisibleColumns: [
      'inboundReceipt.founderName',
      'inboundReceipt.editorName',
      'inboundReceipt.reviewerName',
      'inboundReceipt.createTime',
      'inboundReceipt.reviewTime',
      'inboundReceipt.licensePlate',
      'inboundReceipt.shipmentNumber'
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

const basicClass = computed(() => crud.query ? crud.query.basicClass : undefined)

// 采购单详情
const { detailRef: purchaseOrderRef, openDetail: openPurchaseOrderDetail } = useOtherCrudDetail()
// 入库单详情
const { detailRef: inboundReceiptDetailRef, openDetail: openInboundReceiptDetail } = useOtherCrudDetail()

// 处理刷新
CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content)
  data.content.forEach((row) => {
    if (!row.inboundReceipt) row.inboundReceipt = {}
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
.report-material-inbound-details {
  .el-table {
    ::v-deep(.cell) {
      height: 28px;
      line-height: 28px;
    }
  }
}
</style>
