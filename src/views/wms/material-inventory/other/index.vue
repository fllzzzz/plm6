<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader ref="headerRef" :tableSelections="tableSelections" @clearTableSelection="clearTableSelection" @getSelections="getSelections" @handleClearSelection="handleClearSelection" />
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
      @sort-change="crud.handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
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
      <material-secondary-info-columns :columns="columns" :basic-class="basicClass" />
      <!-- 仓库信息 -->
      <warehouse-info-columns :columns="columns" :show-project="showProjectInfo" :show-monomer="showProjectInfo" :show-area="showProjectInfo" :show-workshop="showProjectInfo" />
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.outbound, ...permission.transfer])"
        label="操作"
        width="120px"
        align="center"
        fixed="right"
      >
        <template #default="{ row: { sourceRow: row } }">
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
import { getOtherInventory } from '@/api/wms/material-inventory'
import { otherMaterialWarehousePM as permission } from '@/page-permission/wms'

import { ref, computed, nextTick } from 'vue'
import { rawMatClsEnum } from '@enum-ms/classification'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import { materialOperateColumns } from '@/utils/columns-format/wms'
import checkPermission from '@/utils/system/check-permission'
import { isNotBlank } from '@data-type/index'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'

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
const tableSelections = ref([])
const alreadyExitArr = ref([])

// 表格列数据格式转换
const columnsDataFormat = ref([...materialOperateColumns])
const { CRUD, crud, columns } = useCRUD(
  {
    title: '其它物料仓',
    sort: ['id.desc'],
    invisibleColumns: [],
    requiredQuery: ['basicClass'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get: getOtherInventory }
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
} = useIndexInfo({ CRUD, crud, defaultBasicClass: rawMatClsEnum.OTHER.V })

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
</script>
