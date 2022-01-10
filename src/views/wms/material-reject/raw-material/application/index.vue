<template>
  <div class="app-container">
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
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id">
        <template #default="{ row }">
          <p>关联项目：<span v-parse-project="{ project: row.projects }" v-empty-text /></p>
          <!-- TODO:入库单增加备注？ -->
          <!-- <p>
            备注：<span v-empty-text>{{ row.remark }}</span>
          </p> -->
          <p>
            审批意见：<span v-empty-text>{{ row.approvalComments }}</span>
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
      />
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
        label="入库申请人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('editorName')"
        key="editorName"
        :show-overflow-tooltip="true"
        prop="editorName"
        label="入库编辑人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('reviewerName')"
        key="reviewerName"
        :show-overflow-tooltip="true"
        prop="reviewerName"
        label="入库审核人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="入库办理日期"
        align="center"
        width="140"
      >
        <template #default="{ row }">
          <span v-parse-time>{{ row.createTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('userUpdateTime')"
        key="userUpdateTime"
        :show-overflow-tooltip="true"
        prop="userUpdateTime"
        label="入库编辑日期"
        align="center"
        width="140"
      >
        <template #default="{ row }">
          <span v-parse-time>{{ row.userUpdateTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('reviewTime')"
        key="reviewTime"
        :show-overflow-tooltip="true"
        prop="reviewTime"
        label="入库审核日期"
        align="center"
        width="140"
      >
        <template #default="{ row }">
          <span v-parse-time>{{ row.reviewTime }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="操作" width="170" align="center" fixed="right">
        <template #default="{ row }">
          <udOperation :data="row" show-detail :show-edit="false" :show-del="false" />
          <common-button type="primary" size="mini" @click="handleRejectApplication(row)">退货办理</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 查看详情 -->
    <m-detail />
    <!-- 退货办理页面 -->
    <m-application v-model:visible="rejectApplicationVisible" :inbound-id="currentRowId" @success="crud.refresh" />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-reject/raw-material/application'
import { ref } from 'vue'
import { rawMatClsEnum } from '@enum-ms/classification'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import UdOperation from '@crud/UD.operation.vue'
import Pagination from '@crud/Pagination'
import MHeader from './module/header.vue'
import MDetail from './module/detail.vue'
import MApplication from './module/application.vue'

// crud交由presenter持有
const permission = {
  get: ['wms_rejectApplication:get'],
  edit: ['wms_rejectApplication:edit'],
  del: ['wms_rejectApplication:del']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const expandRowKeys = ref([])
const rejectApplicationVisible = ref(false)
const tableRef = ref()
const currentRowId = ref()

const { crud, columns } = useCRUD(
  {
    title: '退货办理',
    sort: ['id.desc'],
    invisibleColumns: ['founderName', 'reviewerName', 'reviewTime', 'editorName', 'userUpdateTime', 'licensePlate'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

function handleRejectApplication(row) {
  rejectApplicationVisible.value = true
  currentRowId.value = row.id
}
</script>