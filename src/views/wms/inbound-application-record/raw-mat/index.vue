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
          <!-- <p>
            备注：<span v-empty-text>{{ row.remark }}</span>
          </p> -->
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
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('materialTypeText')"
        key="materialTypeText"
        :show-overflow-tooltip="true"
        prop="materialTypeText"
        label="物料种类"
        min-width="170"
      >
        <template #default="{ row }">
          <span v-parse-enum="{ e: rawMatClsEnum, v: row.basicClass, bit: true, split: ' | ' }" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('projects')" show-overflow-tooltip key="projects" prop="projects" label="关联项目" min-width="170">
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
        v-if="columns.visible('editorName')"
        key="editorName"
        :show-overflow-tooltip="true"
        prop="editorName"
        label="修改人"
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
        label="创建日期"
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
        label="编辑日期"
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
        label="审核日期"
        align="center"
        width="140"
      >
        <template #default="{ row }">
          <span v-parse-time v-empty-text>{{ row.reviewTime }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('reviewStatus')"
        key="reviewStatus"
        :show-overflow-tooltip="true"
        prop="reviewStatus"
        label="状态"
        align="center"
      >
        <template #default="{ row }">
          <el-tag :type="reviewStatusEnum.V[row.reviewStatus].TAG">{{ reviewStatusEnum.VL[row.reviewStatus] }}</el-tag>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="permission.edit" label="操作" width="200px" align="center" fixed="right">
        <template #default="{ row }">
          <udOperation :disabled-edit="!row.editable" :data="row" show-detail />
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
import { ref } from 'vue'
import crudApi from '@/api/wms/inbound/raw-mat-application-record'
import { rawMatClsEnum } from '@enum-ms/classification'
import { reviewStatusEnum } from '@enum-ms/common'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import mHeader from './module/header'
import udOperation from '@crud/UD.operation.vue'
import pagination from '@crud/Pagination'
import mDetail from './module/detail.vue'

// crud交由presenter持有
const permission = {
  get: ['wms_inboundApplication_record:get'],
  edit: ['wms_inboundApplication_record:edit']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '入库记录',
    sort: ['id.desc'],
    invisibleColumns: ['editorName', 'reviewerName', 'userUpdateTime', 'licensePlate'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const expandRowKeys = ref([])
const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach((v) => {
    v.editable = v.reviewStatus !== reviewStatusEnum.PASS.V // 可编辑的
  })
}
</script>
