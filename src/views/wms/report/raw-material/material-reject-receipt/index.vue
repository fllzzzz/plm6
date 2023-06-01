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
      highlight-current-row
      :expand-row-keys="expandRowKeys"
      row-key="id"
      @sort-change="crud.handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <p>
            关联项目：<span>{{ row.projectsFullName }}</span>
          </p>
          <p>
            备注：<span v-empty-text>{{ row.remark }}</span>
          </p>
        </template>
      </el-expand-table-column>
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('rejectTime')"
        key="rejectTime"
        :show-overflow-tooltip="true"
        prop="rejectTime"
        label="退货时间"
        align="center"
        width="140"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        min-width="160"
        label="退货单号"
        align="left"
      />
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
        label="物料种类"
        width="120"
      />
      <el-table-column
        v-if="columns.visible('projects')"
        show-overflow-tooltip
        key="projects"
        prop="projects"
        label="关联项目"
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
      <template v-if="showAmount">
        <el-table-column
          v-if="columns.visible('inboundAmountExcludingVAT')"
          key="inboundAmountExcludingVAT"
          :show-overflow-tooltip="true"
          prop="inboundAmountExcludingVAT"
          label="入库金额(不含税)"
          min-width="120"
          align="right"
        />
        <el-table-column
          v-if="columns.visible('rejectAmountExcludingVAT')"
          key="rejectAmountExcludingVAT"
          :show-overflow-tooltip="true"
          prop="rejectAmountExcludingVAT"
          label="本次退货金额(不含税)"
          width="140"
          align="right"
        />
      </template>
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
      <!--编辑与删除-->
      <el-table-column label="操作" width="75px" align="center" fixed="right">
        <template #default="{ row }">
          <ud-operation :data="row" :show-edit="false" :show-del="false" show-detail />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 查看详情 -->
    <m-detail />
  </div>
</template>

<script setup>
import { getReceiptList as get, getReceiptDetail as detail } from '@/api/wms/report/raw-material/reject'
import { reportRawMaterialRejectReceiptPM as permission } from '@/page-permission/wms'

import { computed, ref } from 'vue'
import { rawMatClsEnum } from '@enum-ms/classification'
import { reviewTimeColumns } from '@/utils/columns-format/wms'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import UdOperation from '@crud/UD.operation.vue'
import Pagination from '@crud/Pagination'
import MHeader from './module/header.vue'
import MDetail from './module/detail.vue'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
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
  ...reviewTimeColumns,
  ['rejectTime', 'parse-time'],
  ['rejectAmountExcludingVAT', ['to-thousand-ck', 'YUAN']],
  ['inboundAmountExcludingVAT', ['to-thousand-ck', 'YUAN']],
  ['projects', ['parse-project', { onlyShortName: true }]],
  ['projectsFullName', 'parse-project', { source: 'projects' }],
  ['basicClass', ['parse-enum', rawMatClsEnum, { bit: true, split: ' | ' }]]
])

const { crud, columns } = useCRUD(
  {
    title: '退货记录',
    sort: ['id.desc'],
    invisibleColumns: ['applicantName', 'reviewerName', 'createTime', 'reviewTime'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get, detail }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 是否有显示金额权限
const showAmount = computed(() => checkPermission(permission.showAmount))
</script>
