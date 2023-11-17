<template>
  <div class="app-container">
    <!--工具栏-->
    <m-header ref="headerRef" :tableSelections="tableSelections" @clearTableSelection="clearTableSelection" @getSelections="getSelections" @handleClearSelection="handleClearSelection"/>
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
      @selection-change="handleChange"
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
        @refresh="crud.toQuery"
      />
      <!-- 单位及其数量 -->
      <material-unit-operate-quantity-columns :columns="columns" :basic-class="basicClass" equal-disabled />
      <!-- 次要信息 -->
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" show-remark/>
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
          <material-print-button  v-if="checkPermission(permission.labelPrint)" :material="row" />
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
      @success="handleSuccessOut"
    />
    <transfer-handling-form
      v-model:visible="transferHandlingVisible"
      :basic-class="basicClass"
      :material="currentRow"
      @success="handleSuccessTransfer"
    />
  </div>
</template>

<script setup>
import { getSteelPlateInventory } from '@/api/wms/material-inventory'
import { steelMaterialWarehousePM as permission } from '@/page-permission/wms'

import { ref, computed, nextTick } from 'vue'
import { matClsEnum, rawMatClsEnum } from '@enum-ms/classification'
import { materialOperateColumns } from '@/utils/columns-format/wms'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import checkPermission from '@/utils/system/check-permission'
import { tableSummary } from '@/utils/el-extra'
import { isNotBlank } from '@data-type/index'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

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
const tableSelections = ref([])
const alreadyExitArr = ref([])

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
  handleTransferSuccess
} = useIndexInfo({ CRUD, crud, defaultBasicClass: rawMatClsEnum.STEEL_PLATE.V })

const showProjectInfo = computed(() => { // 是否显示项目相关信息
  return crud.query?.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V
})

function clearTableSelection() {
  tableSelections.value = []
  alreadyExitArr.value = []
}

function handleClearSelection() {
  tableRef.value.clearSelection()
}

function handleChange(val) {
  crud.selectionChangeHandler(val)
}
// 刷新前
CRUD.HOOK.beforeToQuery = async (crud) => {
  if (!crud.query.projectId) {
    crud.query.monomerId = undefined
    crud.query.areaId = undefined
  }
}

function getSelections() {
  const list = []
  crud.selections.forEach(v => {
    if (list.findIndex(k => k.id === v.id) < 0) {
      list.push(v)
    }
  })
  alreadyExitArr.value.forEach(v => {
    if (list.findIndex(k => k.id === v.id) < 0) {
      list.push(v)
    }
  })
  tableSelections.value = list
}

CRUD.HOOK.beforeRefresh = async (crud) => {
  const list = []
  crud.selections.forEach(v => {
    if (list.findIndex(k => k.id === v.id) < 0) {
      list.push(v)
    }
  })
  alreadyExitArr.value.forEach(v => {
    if (list.findIndex(k => k.id === v.id) < 0) {
      list.push(v)
    }
  })
  tableSelections.value = list
}

CRUD.HOOK.handleRefresh = async (crud, { data }) => {
  headerRef.value && headerRef.value.updateListNumber()
  await setSpecInfoToList(data.content)
  const allArr = []
  data.content = await numFmtByBasicClass(data.content, {
    toSmallest: false,
    toNum: false
  })
  // TODO:后期考虑由服务端处理
  data.content.forEach(async (v) => {
    v.operableQuantity = v.quantity - (v.frozenQuantity || 0)
    v.operableMete = v.mete - (v.frozenMete || 0)
    if (v.outboundUnitType === measureTypeEnum.MEASURE.V) {
      // 实际在出库中使用的数量
      v.corQuantity = v.quantity // 数量
      v.corFrozenQuantity = v.frozenQuantity // 冻结数量
      v.corOperableQuantity = v.operableQuantity // 可操作数量
    } else {
      // 核算量
      v.corQuantity = v.mete
      v.corFrozenQuantity = v.frozenMete
      v.corOperableQuantity = v.operableMete
    }
    if (Array.isArray(v.projectFrozen)) {
      v.projectFrozenKV = {}
      v.projectFrozenForUnitKV = {}
      // 数据转换
      v.projectFrozen = await numFmtByBasicClass(v.projectFrozen, {
        measureUnit: v.measureUnit,
        accountingUnit: v.accountingUnit,
        accountingPrecision: v.accountingPrecision,
        measurePrecision: v.measurePrecision,
        toSmallest: false,
        toNum: true
      })
      allArr.push(v.projectFrozen)
      v.projectFrozen.forEach((pf) => {
        // 用于普通出库
        v.projectFrozenForUnitKV[pf.projectId] = v.outboundUnitType === measureTypeEnum.MEASURE.V ? pf.quantity : pf.mete
        // 用于批量出库
        v.projectFrozenKV[pf.projectId] = pf
      })
    }
  })
  await Promise.all(allArr)
  await Promise.all(data.content)
  crud.data = data.content
  alreadyExitArr.value = []
  if (tableSelections.value && tableSelections.value.length > 0) {
    tableSelections.value.forEach(v => {
      const findVal = crud.data.find(k => k.id === v.id)
      if (isNotBlank(findVal)) {
        nextTick(() => { tableRef.value?.toggleRowSelection(findVal, true) })
      } else {
        alreadyExitArr.value.push(v)
      }
    })
  }
}

function handleSuccessOut() {
  clearTableSelection()
  handleClearSelection()
  handleOutboundSuccess()
}

function handleSuccessTransfer() {
  clearTableSelection()
  handleClearSelection()
  handleTransferSuccess()
}

function getSummaries(param) {
  return tableSummary(param, { props: ['quantity', 'mete'] })
}
</script>
