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
      :expand-row-keys="expandRowKeys"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <p>
            关联项目：<span>{{ row.projectsFullName }}</span>
          </p>
          <!-- TODO:入库单增加备注？ -->
          <!-- <p>
            备注：<span v-empty-text>{{ row.remark }}</span>
          </p> -->
          <p>
            审批意见：<span>{{ row.approvalComments }}</span>
          </p>
        </template>
      </el-expand-table-column>
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('purchaseSN')"
        key="purchaseSN"
        :show-overflow-tooltip="true"
        prop="purchaseSN"
        label="采购单号"
        min-width="155"
      >
        <template #default="{ row }">
          <table-cell-tag :show="!!row.boolPartyA" name="甲供" type="partyA" :offset="10" />
          {{ row.purchaseSN }}
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
      <el-table-column label="操作" width="170px" align="center" fixed="right">
        <template #default="{ row }">
          <udOperation
            :disabled-edit="!row.editable"
            :disabled-del="row.reviewStatus !== reviewStatusEnum.UNREVIEWED.V"
            :data="row"
            show-detail
          />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 查看详情 -->
    <m-detail />
    <!-- 编辑 -->
    <m-form />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-inbound/raw-material/record'
import { rawMaterialInboundRecordPM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { reviewStatusEnum } from '@enum-ms/common'
import { wmsReceiptColumns } from '@/utils/columns-format/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import mHeader from './module/header'
import udOperation from '@crud/UD.operation.vue'
import pagination from '@crud/Pagination'
import mDetail from './module/detail.vue'
import mForm from './module/form.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const expandRowKeys = ref([])
const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([...wmsReceiptColumns, ['approvalComments', 'empty-text']])
const { crud, columns } = useCRUD(
  {
    title: '入库记录',
    sort: ['id.desc'],
    invisibleColumns: ['editorName', 'userUpdateTime', 'licensePlate', 'shipmentNumber'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
</script>
