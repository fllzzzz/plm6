<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      @row-dblclick="(row) => crud.toDetail(row)"
      @selection-change="crud.selectionChangeHandler"
      @sort-change="crud.handleSortChange"
      row-key="id"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <p>
            关联项目：<span>{{ row.projectsFullName }}</span>
          </p>
        </template>
      </el-expand-table-column>
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="70">
        <template #default="{ row, $index }">
          <table-cell-tag :show="row.boolPrinted" name="已打印" type="printed" />
          <span>{{ $index + 1 }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        width="160"
        label="出库单号"
      />
      <el-table-column
        v-if="columns.visible('basicClass')"
        key="basicClass"
        :show-overflow-tooltip="true"
        prop="basicClass"
        label="物料种类"
        width="200"
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
        label="处理人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('outboundTime')"
        key="outboundTime"
        :show-overflow-tooltip="true"
        prop="outboundTime"
        label="出库日期"
        align="center"
        width="200"
      >
        <template #default="{ row }">
          <span>{{ row.outboundEarliestTime }}</span>
          &nbsp;~&nbsp;
          <span>{{ row.outboundLatestTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="申请日期"
        align="center"
        width="160"
        sortable="custom"
      />
      <el-table-column
        v-if="columns.visible('reviewTime')"
        key="reviewTime"
        :show-overflow-tooltip="true"
        prop="reviewTime"
        label="审核日期"
        align="center"
        width="140"
      />
      <!--编辑与删除-->
      <el-table-column label="操作" width="65px" align="center" fixed="right">
        <template #default="{ row }">
          <udOperation :show-edit="false" :show-del="false" show-detail :data="row" />
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
import crudApi from '@/api/wms/material-outbound/raw-material/record'
import { rawMaterialOutboundRecordPM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { wmsReceiptColumns } from '@/utils/columns-format/wms'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import Pagination from '@crud/Pagination'
import UdOperation from '@crud/UD.operation.vue'
import MHeader from './module/header'
import MDetail from './module/detail.vue'

import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const expandRowKeys = ref([])
const tableRef = ref()

const columnsDataFormat = ref([
  ...wmsReceiptColumns,
  ['outboundEarliestTime', ['parse-time', '{y}-{m}-{d}']],
  ['outboundLatestTime', ['parse-time', '{y}-{m}-{d}']]
])

const { crud, columns } = useCRUD(
  {
    title: '出库记录',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
</script>
