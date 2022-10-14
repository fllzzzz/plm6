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
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      @sort-change="crud.handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <p>
            退货关联项目：<span>{{ row.projectsFullName }}</span>
          </p>
          <p>
            备注：<span>{{ row.remark }}</span>
          </p>
          <p>
            审批意见：<span>{{ row.approvalComments }}</span>
          </p>
        </template>
      </el-expand-table-column>
      <el-table-column type="selection" width="55" align="center" />
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
        label="采购合同编号"
        min-width="155"
      >
        <template #default="{ row }">
          <receipt-sn-clickable :receipt-types="['PURCHASE']" :receipt="row.purchaseOrder" />
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
          <receipt-sn-clickable :receipt-types="['INBOUND']" :receipt="row.inboundReceipt" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('basicClass')"
        key="basicClass"
        :show-overflow-tooltip="true"
        prop="basicClass"
        label="退货物料种类"
        width="120"
      />
      <el-table-column
        v-if="columns.visible('projects')"
        show-overflow-tooltip
        key="projects"
        prop="projects"
        label="退货关联项目"
        min-width="170"
      />
      <el-table-column
        v-if="columns.visible('supplier.name')"
        key="supplier.name"
        :show-overflow-tooltip="true"
        prop="supplier.name"
        label="供应商"
        min-width="200"
      />
      <el-table-column
        v-if="columns.visible('applicantName')"
        key="applicantName"
        :show-overflow-tooltip="true"
        prop="applicantName"
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
      />
      <el-table-column
        v-if="columns.visible('reviewTime')"
        key="reviewTime"
        :show-overflow-tooltip="true"
        prop="reviewTime"
        label="审核时间"
        align="center"
        width="140"
      />
      <el-table-column
        v-if="columns.visible('reviewStatus')"
        key="reviewStatus"
        :show-overflow-tooltip="true"
        prop="reviewStatus"
        label="状态"
        align="center"
        width="80"
        fixed="right"
      >
        <template #default="{ row }">
          <template v-if="row.reviewable">
            <common-button type="warning" icon="el-icon-s-check" size="mini" @click="toReview(row)" />
          </template>
          <template v-else>
            <el-tag :type="reviewStatusEnum.V[row.reviewStatus].TAG">{{ reviewStatusEnum.VL[row.reviewStatus] }}</el-tag>
          </template>
        </template>
      </el-table-column>
      <!--详情-->
      <el-table-column label="操作" width="80" align="center" fixed="right">
        <template #default="{ row }">
          <ud-operation :data="row" :show-edit="false" :show-del="false" show-detail />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 查看详情 -->
    <m-detail />
    <!-- 审核 -->
    <review v-model="reviewVisible" :data="currentRow" @refresh="crud.refresh" />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-reject/raw-material/review'
import { rawMaterialRejectReviewPM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { reviewStatusEnum } from '@enum-ms/common'
import { wmsReceiptColumns } from '@/utils/columns-format/wms'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import UdOperation from '@crud/UD.operation.vue'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'
import MDetail from './module/detail.vue'
import Review from './module/review.vue'

import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const expandRowKeys = ref([])
// 当前审核记录
const currentRow = ref({})
const reviewVisible = ref(false)
// 表格
const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([...wmsReceiptColumns, ['remark', 'empty-text'], ['approvalComments', 'empty-text']])
const { CRUD, crud, columns } = useCRUD(
  {
    title: '退货审核',
    sort: ['id.desc'],
    invisibleColumns: ['reviewTime'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach((v) => {
    v.reviewable = v.reviewStatus === reviewStatusEnum.UNREVIEWED.V && checkPermission(permission.review)
  })
}

// 打开审核
function toReview(row) {
  currentRow.value = row
  reviewVisible.value = true
}
</script>
