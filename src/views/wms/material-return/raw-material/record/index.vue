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
            关联出库单：
            <template v-if="row.outboundList && row.outboundList.length > 0">
              <template v-for="(outbound, ri) in row.outboundList" :key="outbound.id">
                <clickable-permission-span
                  :permission="permission.outboundReceiptDetail"
                  @click="openOutboundDetailView(outbound.id)"
                  :text="outbound.serialNumber"
                />
                <span v-if="ri !== row.outboundList.length - 1">、</span>
              </template>
            </template>
            <template v-else>-</template>
          </p>
          <p>
            审批意见：<span>{{ row.approvalComments }}</span>
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
        label="退库单号"
        align="left"
      />
      <el-table-column
        v-if="columns.visible('basicClass')"
        key="basicClass"
        :show-overflow-tooltip="true"
        prop="basicClass"
        label="物料种类"
        align="center"
        width="80"
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
        v-if="columns.visible('outboundSNList')"
        key="outboundSNList"
        :show-overflow-tooltip="true"
        prop="outboundSNList"
        label="关联出库单"
        min-width="155"
      >
        <template #default="{ row }">
          <template v-if="row.outboundList && row.outboundList.length > 0">
            <template v-for="(outbound, ri) in row.outboundList" :key="outbound.id">
              <clickable-permission-span
                :permission="permission.outboundReceiptDetail"
                @click="openOutboundDetailView(outbound.id)"
                :text="outbound.serialNumber"
              />
              <span v-if="ri !== row.outboundList.length - 1"> 、</span>
            </template>
          </template>
          <template v-else>-</template>
        </template>
      </el-table-column>
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
        label="创建时间"
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
    <!-- 出库详情 -->
    <detail-wrapper ref="outboundDetailRef" :api="getOutboundDetail">
      <outbound-detail />
    </detail-wrapper>
  </div>
</template>

<script setup>
import { detail as getOutboundDetail } from '@/api/wms/material-outbound/raw-material/review'
import crudApi from '@/api/wms/material-return/raw-material/record'
import { rawMaterialReturnRecordPM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { reviewStatusEnum } from '@enum-ms/common'
import { wmsReceiptColumns } from '@/utils/columns-format/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'

import OutboundDetail from '@/views/wms/material-outbound/raw-material/review/module/detail.vue'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ClickablePermissionSpan from '@/components-system/common/clickable-permission-span.vue'

import DetailWrapper from '@crud/detail-wrapper.vue'
import MHeader from './module/header'
import UdOperation from '@crud/UD.operation.vue'
import Pagination from '@crud/Pagination'
import MDetail from './module/detail.vue'
import MForm from './module/form.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const expandRowKeys = ref([])
// 出库详情ref
const outboundDetailRef = ref()
const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([...wmsReceiptColumns, ['approvalComments', 'empty-text']])
const { crud, columns } = useCRUD(
  {
    title: '退库记录',
    sort: ['id.desc'],
    invisibleColumns: ['editorName', 'userUpdateTime'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 打开出库详情窗口
function openOutboundDetailView(outboundId) {
  outboundDetailRef.value.toDetail(outboundId)
}
</script>
