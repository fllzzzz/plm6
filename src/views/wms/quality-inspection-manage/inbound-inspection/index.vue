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
          <p>
            质检备注：<span>{{ row.approvalComments }}</span>
          </p>
        </template>
      </el-expand-table-column>
      <el-table-column label="序号" type="index" align="center" width="60" />
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
        v-if="columns.visible('applicantName')"
        key="applicantName"
        :show-overflow-tooltip="true"
        prop="applicantName"
        label="申请人"
        align="center"
        min-width="100"
      />
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
        v-if="columns.visible('attachments')"
        key="attachments"
        prop="attachments"
        :show-overflow-tooltip="false"
        label="质检图片"
        width="150px"
        align="left"
      >
        <template #default="{ row: { sourceRow: row } }">
          <div class="imgs-box">
            <el-image
              v-for="url in row.attachments"
              :preview-src-list="row.imgUrls"
              :initial-index="1"
              :key="url.id"
              :src="url.tinyImageUrl"
              lazy
            ></el-image>
          </div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('qualityTestingUserName')"
        key="qualityTestingUserName"
        :show-overflow-tooltip="true"
        prop="qualityTestingUserName"
        label="质检人"
        align="center"
        min-width="100"
      />
      <el-table-column
        v-if="columns.visible('qualityTestingTime')"
        key="qualityTestingTime"
        :show-overflow-tooltip="true"
        prop="qualityTestingTime"
        label="质检时间"
        align="center"
        width="140"
      />
      <el-table-column
        v-if="columns.visible('qualityTestingEnum')"
        key="qualityTestingEnum"
        :show-overflow-tooltip="true"
        prop="qualityTestingEnum"
        label="质检状态"
        align="center"
        width="100"
        fixed="right"
      >
        <template #default="{ row }">
          <el-tag :type="inspectionStatusEnum.V[row.qualityTestingEnum].TAG">
            {{ inspectionStatusEnum.V[row.qualityTestingEnum].SL }}
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column label="操作" width="90px" align="center" fixed="right">
        <template #default="{ row }">
          <udOperation :data="row" show-detail :showDel="false" :showEdit="false" />
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
import crudApi, { inspectionDetail } from '@/api/wms/material-inbound/raw-material/record'
import { inboundInspectionPM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { inspectionStatusEnum } from '@enum-ms/wms'
import { wmsReceiptColumns } from '@/utils/columns-format/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import mHeader from './module/header'
import udOperation from '@crud/UD.operation.vue'
import pagination from '@crud/Pagination'
import mDetail from './module/detail.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const expandRowKeys = ref([])
const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([...wmsReceiptColumns, ['qualityTestingTime', 'parse-time'], ['approvalComments', 'empty-text']])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '入库质检记录',
    sort: ['id.desc'],
    invisibleColumns: ['editorName', 'userUpdateTime', 'licensePlate', 'shipmentNumber'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi, detail: inspectionDetail }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  data.content = data.content.map(v => {
    v.imgUrls = v.attachments?.map(o => o.imageUrl) || []
    return v
  })
}
</script>

<style lang="scss" scoped>
.imgs-box {
  & > .el-image {
    width: 50px;
    height: 40px;
    border: 2px solid #dcdfe6;
    border-radius: 6px;
    background-color: white;
    cursor: pointer;
    + .el-image {
      margin-left: -40px;
    }
  }
}
</style>
