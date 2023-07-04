<template>
  <div class="app-container">
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
        </template>
      </el-expand-table-column>
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60">
        <template #default="{ row, $index }">
          <table-cell-tag
            v-if="isNotBlank(row.rejectStatus) && row.rejectStatus !== receiptRejectStatusEnum.NONE.V"
            :name="receiptRejectStatusEnum.VL[row.rejectStatus]"
            :color="receiptRejectStatusEnum.V[row.rejectStatus].COLOR"
          />
          {{ $index + 1 }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('purchaseSN')"
        key="purchaseSN"
        :show-overflow-tooltip="true"
        prop="purchaseSN"
        label="采购合同编号"
        min-width="155"
      >
        <template #default="{ row: { sourceRow: row } }">
          <table-cell-tag :show="!!row.boolPartyA && !!row.purchaseOrder?.id" name="甲供" type="partyA" :offset="10" />
          <receipt-sn-clickable v-if="row.purchaseOrder?.id" :receipt-types="['PURCHASE']" :receipt="row.purchaseOrder" />
          <el-tag v-else type="danger" effect="dark" size="small">甲供</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        min-width="160"
        label="入库单号"
        align="left"
      />
      <el-table-column
        v-if="columns.visible('licensePlate')"
        key="licensePlate"
        :show-overflow-tooltip="true"
        prop="licensePlate"
        label="车牌号"
        align="left"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('shipmentNumber')"
        key="shipmentNumber"
        prop="shipmentNumber"
        label="物流单号"
        align="left"
        min-width="150"
        show-overflow-tooltip
      />
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
          label="退货金额(不含税)"
          min-width="120"
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
        v-if="columns.visible('editorName')"
        key="editorName"
        :show-overflow-tooltip="true"
        prop="editorName"
        label="编辑人"
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
        v-if="columns.visible('inboundTime')"
        key="inboundTime"
        :show-overflow-tooltip="true"
        prop="inboundTime"
        label="入库时间"
        align="center"
        width="140"
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
        v-if="columns.visible('userUpdateTime')"
        key="userUpdateTime"
        :show-overflow-tooltip="true"
        prop="userUpdateTime"
        label="编辑时间"
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
      <el-table-column label="操作" width="70px" align="center" fixed="right">
        <template #default="{ row }">
          <udOperation :data="row" :show-del="false" :show-edit="false" show-detail />
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
import { getReceiptList as get, getReceiptDetail as detail } from '@/api/wms/report/raw-material/inbound'
import { reportRawMaterialInboundReceiptPM as permission } from '@/page-permission/wms'

import { computed, ref } from 'vue'
import { rawMatClsEnum } from '@enum-ms/classification'
import { receiptRejectStatusEnum } from '@enum-ms/wms'
import { isNotBlank } from '@/utils/data-type'
import { reviewTimeColumns } from '@/utils/columns-format/wms'
import checkPermission from '@/utils/system/check-permission'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import UdOperation from '@crud/UD.operation.vue'
import Pagination from '@crud/Pagination'
import MHeader from './module/header'
import MDetail from './module/detail.vue'

import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import TableCellTag from '@comp-common/table-cell-tag/index.vue'
import ReceiptSnClickable from '@/components-system/wms/receipt-sn-clickable'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

// 表格列数据格式转换
const columnsDataFormat = computed(() => {
  return [
    ...reviewTimeColumns,
    ['inboundTime', 'parse-time'],
    ['rejectAmountExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['inboundAmountExcludingVAT', ['to-thousand', decimalPrecision.value.wms]],
    ['projects', ['parse-project', { onlyShortName: true }]],
    ['projectsFullName', 'parse-project', { source: 'projects' }],
    ['basicClass', ['parse-enum', rawMatClsEnum, { bit: true, split: ' | ' }]]
  ]
})

const expandRowKeys = ref([])
const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '入库记录',
    sort: ['id.desc'],
    invisibleColumns: [
      'rejectAmountExcludingVAT',
      'editorName',
      'userUpdateTime',
      'createTime',
      'reviewTime',
      'licensePlate',
      'shipmentNumber'
    ],
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
