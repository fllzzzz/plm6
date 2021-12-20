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
      @selection-change="crud.selectionChangeHandler"
    >
      <el-expand-table-column
        v-if="basicClass === matClsEnum.STEEL_PLATE.V"
        :data="crud.data"
        v-model:expand-row-keys="expandRowKeys"
        row-key="id"
        fixed="left"
      >
        <template #default="{ row }">
          <expand-secondary-info :basic-class="row.basicClass" :row="row" :show-batch-no="false" show-graphics />
        </template>
      </el-expand-table-column>
      <el-table-column type="selection" width="55" align="center" fixed="left" />
      <!-- 基础信息 -->
      <material-base-info-columns :columns="columns" :basic-class="basicClass" show-frozen-tip frozen-viewable fixed="left" @refresh="crud.toQuery" />
      <!-- 单位及其数量 -->
      <material-unit-operate-quantity-columns :columns="columns" :basic-class="basicClass" />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" />
      <warehouse-info-columns :columns="columns" />
      <!--编辑与删除-->
      <el-table-column label="操作" width="180px" align="center" fixed="right">
        <template #default="{ row }">
          <!--出库-->
          <common-button v-permission="permission.outbound" type="primary" size="mini" @click="toOutHandle(row)">
            <svg-icon icon-class="wms-outbound" />
          </common-button>
          <!--调拨-->
          <common-button v-permission="permission.transfer" type="warning" size="mini" @click="toTransfer(row)">
            <svg-icon icon-class="wms-transfer" />
          </common-button>
          <!--打印-->
          <common-button icon="el-icon-printer" type="success" size="mini" @click="toPrintLabel(row)" />
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
import { ref } from 'vue'
import { getSteelPlateInventory } from '@/api/wms/material-inventory'
import { matClsEnum, rawMatClsEnum } from '@enum-ms/classification'

import useCRUD from '@compos/use-crud'
import useIndexInfo from '../compos/use-index-info'
import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitOperateQuantityColumns from '@/components-system/wms/table-columns/material-unit-operate-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import OutboundHandlingForm from '@/views/wms/outbound-components/outbound-handling-form/index.vue'
import TransferHandlingForm from '@/views/wms/transfer-components/transfer-handling-form/index.vue'
import MHeader from './module/header'
import Pagination from '@crud/Pagination'

// crud交由presenter持有
const permission = {
  get: ['wms_matWarehouse_steel:get'],
  outbound: ['wms_matWarehouse_steel:outbound'],
  transfer: ['wms_matWarehouse_steel:transfer'],
  freezeList: ['wms_raw_mat_freeze_list:get']
}

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
    title: '钢材物料仓',
    sort: ['id.desc'],
    invisibleColumns: [],
    requiredQuery: ['basicClass'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get: getSteelPlateInventory }
  },
  tableRef
)

const {
  expandRowKeys,
  maxHeight,
  basicClass,
  currentRow,
  outboundHandlingVisible,
  transferHandlingVisible,
  toTransfer,
  toOutHandle,
  handleOutboundSuccess,
  handleTransferSuccess
} = useIndexInfo({ CRUD, crud, defaultBasicClass: rawMatClsEnum.STEEL_PLATE.V })
</script>
