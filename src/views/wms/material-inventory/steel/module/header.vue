<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <!-- 物料查询相关 -->
      <mat-header-query :basic-class="query.basicClass" :query="query" :to-query="crud.toQuery" show-project-Warehouse-type>
        <template #firstLineRight>
          <span class="child-mr-6">
            <current-user-outbound-list ref="currentUserOutboundListRef" @refresh="crud.toQuery" />
            <common-button icon="el-icon-time" size="mini" type="info" @click="toOutboundRecord">出库记录</common-button>
            <common-button type="info" size="mini" icon="el-icon-lock" @click="openFreezeRecords"> 冻结记录 </common-button>
          </span>
        </template>
        <template #afterProjectWarehouseType>
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
      <rr-operation />
    </div>
    <crud-operation>
      <!-- TODO:打印 -->
      <template #optLeft>
        <common-button class="filter-item" v-permission="permission.outbound" type="primary" size="mini" @click="toBatchOutbound">
          <svg-icon icon-class="wms-outbound" /> 批量出库
        </common-button>
        <common-button class="filter-item" v-permission="permission.transfer" type="warning" size="mini" @click="toBatchTransfer">
          <svg-icon icon-class="wms-transfer" /> 批量调拨
        </common-button>
      </template>
      <template #viewLeft>
        <!-- <common-button class="filter-item" type="success" size="mini" icon="el-icon-printer" @click="toBatchPrint">批量打印</common-button> -->
        <el-badge :value="notPrintedMaterialQuantity" :hidden="notPrintedMaterialQuantity <= 0" style="z-index: 1">
          <common-button class="filter-item" type="primary" size="mini" icon="el-icon-printer" @click="toPrintNotPrintedLabel">
            标签打印
          </common-button>
        </el-badge>
      </template>
    </crud-operation>
    <outbound-batch-handling-form
      v-model:visible="batchOutboundHandlingVisible"
      :project-warehouse-type="query.projectWarehouseType"
      :basic-class="query.basicClass"
      :material-list="crud.selections"
      @success="handleBatchOutbound"
    />
    <transfer-batch-handling-form
      v-model:visible="batchTransferHandlingVisible"
      :basic-class="query.basicClass"
      :material-list="crud.selections"
      @success="handleTransferOutbound"
    />
    <common-drawer title="标签打印" v-model="materialPrintViewVisible" size="90%">
      <template #content>
        <material-label-print-view @printed-success="notPrintedMaterialRefresh" />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { getSteelPlateInventory, getSectionSteelInventory, getSteelCoilInventory } from '@/api/wms/material-inventory'
import { computed, defineExpose, onMounted, ref } from 'vue'
import { steelClsEnum } from '@/utils/enum/modules/classification'

import useHeaderInfo from '../../compos/use-header-info'
import useGetNotPrintedMaterial from '@/composables/store/use-get-not-printed-material'
import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'
import CurrentUserOutboundList from '@/views/wms/material-outbound/raw-material/components/current-user-outbound-list/index.vue'
import OutboundBatchHandlingForm from '@/views/wms/material-outbound/raw-material/components/outbound-batch-handling-form/index.vue'
import TransferBatchHandlingForm from '@/views/wms/material-transfer/raw-material/components/transfer-batch-handling-form/index.vue'
import materialLabelPrintView from '@/views/wms/material-label-print/index.vue'

const {
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
const { notPrintedMaterialNumber, refresh: notPrintedMaterialRefresh } = useGetNotPrintedMaterial()

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

// TODO:
// 批量打印
// function toBatchPrint() {

// }

// 打印 未打印标签的物料
function toPrintNotPrintedLabel() {
  materialPrintViewVisible.value = true
}

defineExpose({
  updateListNumber
})
</script>
