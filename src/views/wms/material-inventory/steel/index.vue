<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header ref="headerRef" />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      :key="`material_inventory_${crud.query.basicClass}`"
      v-loading="crud.loading"
      :data="crud.data"
      :data-format="columnsDataFormat"
      :max-height="maxHeight"
      :default-expand-all="false"
      :expand-row-keys="expandRowKeys"
      row-key="id"
      show-summary
      :summary-method="getSummaries"
      @sort-change="crud.handleSortChange"
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
      <material-base-info-columns
        :columns="columns"
        :basic-class="basicClass"
        show-frozen-tip
        frozen-viewable
        sortable
        fixed="left"
        @refresh="handleRefresh"
      />
      <!-- 单位及其数量 -->
      <material-unit-operate-quantity-columns :columns="columns" :basic-class="basicClass" equal-disabled />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" />
      <!-- 仓库信息 -->
      <warehouse-info-columns :columns="columns" :show-project="showProjectInfo" :show-monomer="showProjectInfo" :show-area="showProjectInfo" />
      <!--编辑与删除-->
      <el-table-column label="操作" width="180px" align="center" fixed="right">
        <template #default="{ row: { sourceRow: row } }">
          <!--出库-->
          <common-button v-permission="permission.outbound" type="primary" size="mini" @click="toOutHandle(row)">
            <svg-icon icon-class="wms-outbound" />
          </common-button>
          <!--调拨-->
          <common-button v-permission="permission.transfer" type="warning" size="mini" @click="toTransfer(row)">
            <svg-icon icon-class="wms-transfer" />
          </common-button>
          <!--打印-->
          <material-print-button :material="row" />
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
import { getSteelPlateInventory } from '@/api/wms/material-inventory'
import { steelMaterialWarehousePM as permission } from '@/page-permission/wms'

import { ref, computed } from 'vue'
import { matClsEnum, rawMatClsEnum } from '@enum-ms/classification'
import { materialOperateColumns } from '@/utils/columns-format/wms'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import { tableSummary } from '@/utils/el-extra'

import useCRUD from '@compos/use-crud'
import useIndexInfo from '../compos/use-index-info'
import OutboundHandlingForm from '@/views/wms/material-outbound/raw-material/components/outbound-handling-form/index.vue'
import TransferHandlingForm from '@/views/wms/material-transfer/raw-material/components/transfer-handling-form/index.vue'
import MHeader from './module/header'
import Pagination from '@crud/Pagination'

import ElExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import ExpandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitOperateQuantityColumns from '@/components-system/wms/table-columns/material-unit-operate-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import materialPrintButton from '@/components-system/wms/material-print-button.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

// 表格ref
const tableRef = ref()
// 表格列数据格式转换
const columnsDataFormat = ref([...materialOperateColumns])
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
} = useIndexInfo({ CRUD, crud, defaultBasicClass: rawMatClsEnum.STEEL_PLATE.V })

const showProjectInfo = computed(() => { // 是否显示项目相关信息
  return crud.query?.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V
})

// 刷新前
CRUD.HOOK.beforeToQuery = async (crud) => {
  if (!crud.query.projectId) {
    crud.query.monomerId = undefined
    crud.query.areaId = undefined
  }
}

function getSummaries(param) {
  return tableSummary(param, { props: ['quantity', 'mete'] })
}
</script>
