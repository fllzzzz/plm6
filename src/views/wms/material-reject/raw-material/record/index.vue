<template>
  <div class="app-container">
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
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <p>关联项目：<span v-parse-project="{ project: row.projects }" v-empty-text /></p>
          <p>
            备注：<span v-empty-text>{{ row.remark }}</span>
          </p>
          <p>
            审批意见：<span v-empty-text>{{ row.approvalComments }}</span>
          </p>
        </template>
      </el-expand-table-column>
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        min-width="160"
        label="退货单号"
        align="left"
      >
        <template #default="{ row }">
          <!-- 解冻 -->
          <table-cell-tag v-if="row.boolHasUnfreeze" name="解冻" type="unfreeze" :offset="15" />
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('purchaseOrder.serialNumber')"
        key="purchaseOrder.serialNumber"
        :show-overflow-tooltip="true"
        prop="purchaseOrder.serialNumber"
        label="采购单号"
        min-width="155"
      >
        <template #default="{ row }">
          <clickable-permission-span
            v-if="row.purchaseOrder"
            :permission="permission.purchaseOrderDetail"
            @click="openPurchaseOrderDetail(row.purchaseOrder.id)"
            :text="row.purchaseOrder.serialNumber"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('inboundReceipt.serialNumber')"
        key="inboundReceipt.serialNumber"
        :show-overflow-tooltip="true"
        prop="inboundReceipt.serialNumber"
        min-width="160"
        label="入库单号"
        align="left"
      >
        <template #default="{ row }">
          <clickable-permission-span
            v-if="row.inboundReceipt"
            :permission="permission.inboundReceiptDetail"
            @click="openInboundDetail(row.inboundReceipt.id)"
            :text="row.inboundReceipt.serialNumber"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('materialTypeText')"
        key="materialTypeText"
        :show-overflow-tooltip="true"
        prop="materialTypeText"
        label="物料种类"
        width="120"
      >
        <template #default="{ row }">
          <span v-parse-enum="{ e: rawMatClsEnum, v: row.basicClass, bit: true, split: ' | ' }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('projects')"
        show-overflow-tooltip
        key="projects"
        prop="projects"
        label="关联项目"
        min-width="170"
      >
        <template #default="{ row }">
          <span v-parse-project="{ project: row.projects, onlyShortName: true }" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('supplier.name')"
        key="supplier.name"
        :show-overflow-tooltip="true"
        prop="supplier.name"
        label="供应商"
        min-width="200"
      />
      <el-table-column
        v-if="columns.visible('founderName')"
        key="founderName"
        :show-overflow-tooltip="true"
        prop="founderName"
        label="申请人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('reviewerName')"
        key="reviewerName"
        :show-overflow-tooltip="true"
        prop="reviewerName"
        label="审核人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="申请时间"
        align="center"
        width="140"
      >
        <template #default="{ row }">
          <span v-parse-time="row.createTime" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('reviewTime')"
        key="reviewTime"
        :show-overflow-tooltip="true"
        prop="reviewTime"
        label="审核时间"
        align="center"
        width="140"
      >
        <template #default="{ row }">
          <span v-parse-time="row.reviewTime" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('reviewStatus')"
        key="reviewStatus"
        :show-overflow-tooltip="true"
        prop="reviewStatus"
        label="状态"
        align="center"
        fixed="right"
      >
        <template #default="{ row }">
          <el-tag :type="reviewStatusEnum.V[row.reviewStatus].TAG">{{ reviewStatusEnum.VL[row.reviewStatus] }}</el-tag>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="操作" width="120px" align="center" fixed="right">
        <template #default="{ row }">
          <ud-operation :disabled-del="row.reviewStatus !== reviewStatusEnum.UNREVIEWED.V" :data="row" :show-edit="false" show-detail />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 查看详情 -->
    <m-detail />
    <!-- 入库单详情 -->
    <detail-wrapper ref="inboundDetailRef" :api="getInboundDetail">
      <inbound-detail />
    </detail-wrapper>
    <!-- 采购订单详情 -->
    <detail-wrapper ref="purchaseOrderRef" :api="getPurchaseOrderDetail">
      <purchase-order-detail />
    </detail-wrapper>
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-reject/raw-material/record'
import { detail as getInboundDetail } from '@/api/wms/material-inbound/raw-material/review'
import { detail as getPurchaseOrderDetail } from '@/api/wms/purchase-order'
import { rawMaterialRejectRecordPM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { rawMatClsEnum } from '@enum-ms/classification'
import { reviewStatusEnum } from '@enum-ms/common'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useOtherCrudDetail from '@compos/use-other-crud-detail'

import UdOperation from '@crud/UD.operation.vue'
import Pagination from '@crud/Pagination'
import DetailWrapper from '@crud/detail-wrapper.vue'
import MHeader from './module/header.vue'
import MDetail from './module/detail.vue'

import InboundDetail from '@/views/wms/material-inbound/raw-material/review/module/detail.vue'
import purchaseOrderDetail from '@/views/wms/purchase-order/module/detail.vue'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ClickablePermissionSpan from '@/components-system/common/clickable-permission-span.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const expandRowKeys = ref([])
const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '退货记录',
    sort: ['id.desc'],
    invisibleColumns: ['reviewTime'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const { detailRef: inboundDetailRef, openDetail: openInboundDetail } = useOtherCrudDetail()
const { detailRef: purchaseOrderRef, openDetail: openPurchaseOrderDetail } = useOtherCrudDetail()
</script>
