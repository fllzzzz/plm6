<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <!-- 物料查询相关 -->
      <mat-header-query :basic-class="query.basicClass" :query="query" :to-query="crud.toQuery" @searchQuery="searchQuery" multiple show-project-Warehouse-type>
        <template #afterProjectWarehouseType>
          <monomer-select-area-select
            v-if="query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V"
            v-model:monomerId="query.monomerId"
            v-model:areaId="query.areaId"
            clearable
            areaClearable
            :filterArea="false"
            :project-id="query.projectId"
            :monomerDisabled="!query.projectId"
            :areaDisabled="!query.projectId"
            @change="crud.toQuery()"
          />
          <common-radio-button
            v-model="query.basicClass"
            :options="steelClsEnum.ENUM"
            type="enum"
            size="small"
            class="filter-item"
            @change="handleBasicClassChange"
          />
        </template>
      </mat-header-query>
      <rr-operation @resetClick="resetClick" />
    </div>
    <crud-operation>
      <!-- TODO:打印 -->
      <template #optLeft>
        <common-button class="filter-item" v-permission="permission.outbound" type="primary" size="mini" @click="handleOut">
          <svg-icon icon-class="wms-outbound" /> 批量出库
        </common-button>
        <common-button class="filter-item" v-permission="permission.transfer" type="warning" size="mini" @click="handleTransfer">
          <svg-icon icon-class="wms-transfer" /> 批量调拨
        </common-button>
      </template>
      <template #viewLeft>
        <!-- <common-button class="filter-item" type="success" size="mini" icon="el-icon-printer" @click="toBatchPrint">批量打印</common-button> -->
        <el-badge v-permission="permission.convertListBtn" :value="unAuditNum" :hidden="unAuditNum <= 0" style="z-index: 2">
          <common-button class="filter-item" type="warning" size="mini" @click="convertClick">条板转换清单</common-button>
        </el-badge>
        <el-badge v-permission="permission.labelPrint" :value="notPrintedMaterialQuantity" :hidden="notPrintedMaterialQuantity <= 0" style="z-index: 1">
          <common-button class="filter-item" type="primary" size="mini" icon="el-icon-printer" @click="toPrintNotPrintedLabel">
            标签打印
          </common-button>
        </el-badge>
        <current-user-outbound-list v-if="checkPermission(permission.outbound)" ref="currentUserOutboundListRef" @refresh="handleCurrentUserOutbound" />
        <common-button v-permission="permission.outboundRecord" class="filter-item" icon="el-icon-time" size="mini" type="info" @click="toOutboundRecord">出库记录</common-button>
        <common-button v-permission="permission.freezeRecord" class="filter-item" type="info" size="mini" icon="el-icon-lock" @click="openFreezeRecords"> 冻结记录 </common-button>
      </template>
    </crud-operation>
    <outbound-batch-handling-form
      v-model:visible="batchOutboundHandlingVisible"
      :project-warehouse-type="query.projectWarehouseType"
      :project-id="query.projectId"
      :basic-class="query.basicClass"
      :material-list="props.tableSelections"
      @success="handleSuccessOut"
    />
    <transfer-batch-handling-form
      v-model:visible="batchTransferHandlingVisible"
      :basic-class="query.basicClass"
      :material-list="props.tableSelections"
      @success="handleSuccessTransfer"
    />
    <common-drawer title="标签打印" v-model="materialPrintViewVisible" size="90%">
      <template #content>
        <material-label-print-view @printed-success="notPrintedMaterialRefresh" />
      </template>
    </common-drawer>
    <common-drawer title="条板转换列表" v-model="convertConfirmVisible" size="95%">
      <template #content>
        <convert-confirm-list @success="fetchAuditNum();crud.toQuery()" :showType="'coilPlate'" :visible="convertConfirmVisible"/>
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { auditNum } from '@/api/wms/report/raw-material/convert-list'
import { getSteelPlateInventory, getSectionSteelInventory, getSteelCoilInventory } from '@/api/wms/material-inventory'
import { computed, defineExpose, onMounted, ref, defineProps, defineEmits } from 'vue'
import { steelClsEnum } from '@/utils/enum/modules/classification'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'
import checkPermission from '@/utils/system/check-permission'

import useHeaderInfo from '../../compos/use-header-info'
import useGetNotPrintedMaterial from '@/composables/store/use-get-not-printed-material'
import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'
import CurrentUserOutboundList from '@/views/wms/material-outbound/raw-material/components/current-user-outbound-list/index.vue'
import OutboundBatchHandlingForm from '@/views/wms/material-outbound/raw-material/components/outbound-batch-handling-form/index.vue'
import TransferBatchHandlingForm from '@/views/wms/material-transfer/raw-material/components/transfer-batch-handling-form/index.vue'
import materialLabelPrintView from '@/views/wms/material-label-print/index.vue'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import convertConfirmList from './convert-confirm-list/index.vue'

const {
  CRUD,
  crud,
  query,
  permission,
  currentUserOutboundListRef,
  batchOutboundHandlingVisible,
  batchTransferHandlingVisible,
  updateListNumber,
  toOutboundRecord,
  toBatchOutbound,
  handleBatchOutbound,
  toBatchTransfer,
  handleTransferOutbound,
  openFreezeRecords
} = useHeaderInfo({ defaultBasicClass: steelClsEnum.STEEL_PLATE.V })

const materialPrintViewVisible = ref(false)
const convertConfirmVisible = ref(false)
const unAuditNum = ref(0)
const { notPrintedMaterialNumber, refresh: notPrintedMaterialRefresh } = useGetNotPrintedMaterial()

const props = defineProps({
  tableSelections: {
    type: Array,
    default: () => []
  }
})

const emit = defineEmits(['clearTableSelection', 'getSelections', 'handleClearSelection'])

// 未打印的标签数量
const notPrintedMaterialQuantity = computed(() => {
  if (notPrintedMaterialNumber.value) {
    return notPrintedMaterialNumber.value.totalMaterial
  } else {
    return 0
  }
})

onMounted(async () => {
  // notPrintedMaterialQuantity.value = await getDetailNumberByCurrentUser()
})

// CRUD.HOOK.handleRefresh = async (crud, { data }) => {
//   notPrintedMaterialQuantity.value = data.notPrintedMaterialQuantity || 0
// }

function handleOut() {
  emit('getSelections')
  toBatchOutbound()
}

function handleTransfer() {
  emit('getSelections')
  toBatchTransfer()
}

function searchQuery() {
  emit('clearTableSelection')
  emit('handleClearSelection')
}

function handleSuccessOut() {
  emit('clearTableSelection')
  emit('handleClearSelection')
  handleBatchOutbound()
}

function handleSuccessTransfer() {
  emit('clearTableSelection')
  emit('handleClearSelection')
  handleTransferOutbound()
}

// 基础类型发生变化
async function handleBasicClassChange(val) {
  switch (val) {
    case steelClsEnum.STEEL_PLATE.V:
      crud.crudApi.get = getSteelPlateInventory
      break
    case steelClsEnum.SECTION_STEEL.V:
      crud.crudApi.get = getSectionSteelInventory
      break
    case steelClsEnum.STEEL_COIL.V:
      crud.crudApi.get = getSteelCoilInventory
      break
  }
  await crud.resetQuery()
  crud.data = []
  crud.setColumns()
}

function resetClick() {
  emit('clearTableSelection')
  emit('handleClearSelection')
}

// 处理当前用户出库成功
function handleCurrentUserOutbound() {
  crud.refresh()
  notPrintedMaterialRefresh()
}

// 打印 未打印标签的物料
function toPrintNotPrintedLabel() {
  materialPrintViewVisible.value = true
}

function convertClick() {
  convertConfirmVisible.value = true
}

CRUD.HOOK.afterRefresh = async (crud) => {
  if (crud.query.basicClass === steelClsEnum.STEEL_COIL.V) {
    fetchAuditNum()
  }
}

async function fetchAuditNum() {
  unAuditNum.value = 0
  try {
    unAuditNum.value = await auditNum() || 0
  } catch (error) {
    console.log('未审核数量', error)
  }
}

function headerRefreshNum() {
  fetchAuditNum()
  updateListNumber()
}

defineExpose({
  updateListNumber,
  headerRefreshNum
})
</script>
