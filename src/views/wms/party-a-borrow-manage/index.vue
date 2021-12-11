<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header />
    <!--表格渲染-->
    <common-table
      :key="`party_a_borrow_${crud.query.basicClass}`"
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      @sort-change="crud.handleSortChange"
    >
      <el-expand-table-column :data="crud.data" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
        <template #default="{ row }">
          <expand-secondary-info :basic-class="row.basicClass" :row="row" show-graphics>
            <p>
              调拨备注：<span v-empty-text>{{ row.remark }}</span>
            </p>
          </expand-secondary-info>
        </template>
      </el-expand-table-column>
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="crud.query.basicClass" :show-party-a="false" />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" />
      <!-- 单位及其数量 -->
      <material-unit-quantity-columns :columns="columns" outbound-type-mode />
      <el-table-column v-if="columns.visible('project')" show-overflow-tooltip key="project" prop="project" label="原项目" min-width="170">
        <template #default="{ row }">
          <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('borrowProject')"
        show-overflow-tooltip
        key="borrowProject"
        prop="borrowProject"
        label="借用项目"
        min-width="170"
      >
        <template #default="{ row }">
          <span v-parse-project="{ project: row.borrowProject, onlyShortName: true }" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('transferSN')"
        key="transferSN"
        :show-overflow-tooltip="true"
        prop="transferSN"
        label="调拨单号"
        align="center"
        width="170"
      >
        <template #default="{ row }">
          <clickable-permission-span :permission="permission.transferDetail" @click="openTransferDetailView(row.transferId)">
            {{ row.transferSN }}
          </clickable-permission-span>
          <!-- <span class="text-clickable" @click="openTransferDetailView(row.transferId)">{{ row.transferSN }}</span> -->
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('transferorName')"
        key="transferorName"
        :show-overflow-tooltip="true"
        prop="transferorName"
        label="调拨人"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('transferTime')"
        key="transferTime"
        :show-overflow-tooltip="true"
        prop="transferTime"
        label="调拨日期"
        align="center"
        width="100"
        sortable="custom"
      >
        <template #default="{ row }">
          <span v-parse-time="'{y}-{m}-{d}'">{{ row.transferTime }}</span>
        </template>
      </el-table-column>
      <!-- 归还 -->
      <el-table-column label="操作" width="80px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button type="primary" size="mini" @click="toReturn(row)">归还</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <detail-wrapper ref="transferDetailRef" :api="getTransferDetail">
      <transfer-detail />
    </detail-wrapper>
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/transfer/party-a-borrow-manage'
import { detail as getTransferDetail } from '@/api/wms/transfer/raw-mat-application-review'

import { ref } from 'vue'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import Pagination from '@crud/Pagination'
import DetailWrapper from '@crud/detail-wrapper.vue'
import MHeader from './module/header'

import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import TransferDetail from '@/views/wms/transfer-application-review/raw-mat/module/detail.vue'
import ClickablePermissionSpan from '@/components-system/common/clickable-permission-span.vue'

const permission = {
  get: ['wms_partyABorrow:get'],
  return: ['wms_partyABorrow:return'],
  transferDetail: ['wms_transferApplication_review:detail']
}

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const transferDetailRef = ref()
const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '甲供物料借出管理',
    sort: ['id.desc'],
    invisibleColumns: ['transferorName', 'transferTime'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const expandRowKeys = ref([])
const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  await setSpecInfoToList(data.content)
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: false
  })
}

// 打开入库详情窗口
function openTransferDetailView(transferId) {
  transferDetailRef.value.toDetail(transferId)
}

// 归还
function toReturn() {}
</script>
