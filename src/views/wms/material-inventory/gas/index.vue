<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headerRef" />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      :key="`material_inventory_${crud.query.basicClass}`"
      v-loading="crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      @sort-change="crud.handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" fixed="left" />
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="basicClass" show-frozen-tip frozen-viewable sortable fixed="left" @refresh="handleRefresh" />
      <!-- 单位及其数量 -->
      <material-unit-operate-quantity-columns :columns="columns" :basic-class="basicClass" equal-disabled />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" />
      <!-- 仓库信息 -->
      <warehouse-info-columns :columns="columns" />
      <!--编辑与删除-->
      <el-table-column label="操作" width="120px" align="center" fixed="right">
        <template #default="{ row }">
          <!--出库-->
          <common-button v-permission="permission.outbound" type="primary" size="mini" @click="toOutHandle(row)">
            <svg-icon icon-class="wms-outbound" />
          </common-button>
          <!--调拨-->
          <common-button v-permission="permission.transfer" type="warning" size="mini" @click="toTransfer(row)">
            <svg-icon icon-class="wms-transfer" />
          </common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 出库办理表单 -->
    <outbound-handling-form
      v-model:visible="outboundHandlingVisible"
      :basic-class="basicClass"
      :material="currentRow"
      @success="handleOutboundSuccess"
    />
    <transfer-handling-form
      v-model:visible="transferHandlingVisible"
      :basic-class="basicClass"
      :material="currentRow"
      @success="handleTransferSuccess"
    />
  </div>
</template>

<script setup>
import { getGasInventory } from '@/api/wms/material-inventory'
import { gasMaterialWarehousePM as permission } from '@/page-permission/wms'

import { ref } from 'vue'
import { rawMatClsEnum } from '@enum-ms/classification'

import useIndexInfo from '../compos/use-index-info'
import useCRUD from '@compos/use-crud'
import MHeader from './module/header'
import Pagination from '@crud/Pagination'
import OutboundHandlingForm from '@/views/wms/material-outbound/raw-material/components/outbound-handling-form/index.vue'
import TransferHandlingForm from '@/views/wms/material-transfer/raw-material/components/transfer-handling-form/index.vue'

import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitOperateQuantityColumns from '@/components-system/wms/table-columns/material-unit-operate-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

// 表格ref
const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '气体物料仓',
    sort: ['id.desc'],
    invisibleColumns: [],
    requiredQuery: ['basicClass'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get: getGasInventory }
  },
  tableRef
)

const {
  headerRef,
  expandRowKeys,
  maxHeight,
  basicClass,
  currentRow,
  outboundHandlingVisible,
  transferHandlingVisible,
  toTransfer,
  toOutHandle,
  handleOutboundSuccess,
  handleTransferSuccess,
  handleRefresh
} = useIndexInfo({ CRUD, crud, defaultBasicClass: rawMatClsEnum.GAS.V })
</script>
