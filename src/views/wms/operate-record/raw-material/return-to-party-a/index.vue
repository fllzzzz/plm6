<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      :key="`return_to_party_a_${crud.query.basicClass}`"
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      @sort-change="crud.handleSortChange"
    >
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="crud.query.basicClass" :show-party-a="false" fixed="left" />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="crud.query.basicClass" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" :basic-class="crud.query.basicClass" />
      <!-- 仓库信息 -->
      <warehouse-info-columns :columns="columns" show-project />
      <el-table-column
        v-if="columns.visible('transferSN')"
        key="transferSN"
        :show-overflow-tooltip="true"
        prop="transferSN"
        label="调拨单号"
        align="center"
        min-width="120"
      >
        <template #default="{ row }">
          <clickable-permission-span
            v-if="row.transfer"
            :permission="permission.transferReceiptDetail"
            @click="openTransferDetailView(row.transfer.id)"
            :text="row.transfer.serialNumber"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('applicantName')"
        key="applicantName"
        :show-overflow-tooltip="true"
        prop="applicantName"
        label="操作人"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="归还日期"
        align="center"
        width="100"
        sortable="custom"
      >
        <template #default="{ row }">
          <span v-parse-time="{ val: row.createTime, fmt: '{y}-{m}-{d}' }" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 调拨详情 -->
    <detail-wrapper ref="transferDetailRef" :api="getTransferDetail">
      <transfer-detail />
    </detail-wrapper>
    <!-- -->
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/material-transfer/raw-material/return-to-party-a'
import { detail as getTransferDetail } from '@/api/wms/material-transfer/raw-material/review'
import { operateRecordReturnToPartyAPM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import DetailWrapper from '@crud/detail-wrapper.vue'
import MHeader from './module/header'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import TransferDetail from '@/views/wms/material-transfer/raw-material/review/module/detail.vue'
import ClickablePermissionSpan from '@/components-system/common/clickable-permission-span.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

// 展开行
const expandRowKeys = ref([])
// 调拨详情ref
const transferDetailRef = ref()
// 表格ref
const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '归还甲方',
    sort: ['id.desc'],
    invisibleColumns: ['heatNoAndBatchNo', 'warehouse', 'transferSN', 'applicantName'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: false
  })
}

// 打开调拨详情窗口
function openTransferDetailView(transferId) {
  transferDetailRef.value.toDetail(transferId)
}
</script>
