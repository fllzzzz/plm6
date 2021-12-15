<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <!-- 物料查询相关 -->
      <mat-header-query :basic-class="query.basicClass" :query="query" :to-query="crud.toQuery" show-project-Warehouse-type>
        <template #firstLineRight>
          <current-user-outbound-list ref="currentUserOutboundListRef" />
          <common-button icon="el-icon-time" size="mini" type="info" @click="toOutboundRecord">出库记录</common-button>
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
        <common-button
          v-permission="permission.get"
          class="filter-item"
          type="info"
          size="mini"
          icon="el-icon-lock"
          @click="openFreezeRecords"
        >
          冻结记录
        </common-button>
        <common-button class="filter-item" type="success" size="mini" icon="el-icon-printer" @click="toBatchPrint">批量打印</common-button>
        <el-badge :value="notPrintedMaterialQuantity" :hidden="notPrintedMaterialQuantity <= 0">
          <common-button class="filter-item" type="primary" size="mini" icon="el-icon-printer" @click="toPrintNotPrintedLabel">
            新入库标签打印
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
  </div>
</template>

<script setup>
import { getSteelPlateInventory, getSectionSteelInventory, getSteelCoilInventory } from '@/api/wms/material-inventory'
import { defineExpose, inject, watch, onMounted, ref } from 'vue'
import { useRouter } from 'vue-router'
import { mapGetters } from '@/store/lib'
import { steelClsEnum } from '@/utils/enum/modules/classification'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'

import { regHeader } from '@compos/use-crud'
import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'
import CurrentUserOutboundList from '@/views/wms/outbound-components/current-user-outbound-list/index.vue'
import OutboundBatchHandlingForm from '@/views/wms/outbound-components/outbound-batch-handling-form/index.vue'
import TransferBatchHandlingForm from '@/views/wms/transfer-components/transfer-batch-handling-form/index.vue'

const router = useRouter()
const permission = inject('permission')

// 查询参数
const defaultQuery = {
  projectId: { value: undefined, resetAble: false }, // 项目id
  projectWarehouseType: { value: projectWarehouseTypeEnum.PUBLIC.V, resetAble: false },
  basicClass: { value: steelClsEnum.STEEL_PLATE.V, resetAble: false }
}

const { crud, query } = regHeader(defaultQuery)

// 出库清单组件
const currentUserOutboundListRef = ref()
// 显示批量出库
const batchOutboundHandlingVisible = ref(false)
// 显示批量调拨
const batchTransferHandlingVisible = ref(false)

// 未打印的标签数量
const notPrintedMaterialQuantity = ref(0)

// 全局项目id
const { globalProjectId } = mapGetters('globalProjectId')
// 选中项目库时， 根据项目id的变化刷新列表
watch(
  globalProjectId,
  (val) => {
    if (query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V) {
      crud.query.projectId = val
      crud.toQuery()
    }
  },
  { immediate: true }
)

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

// 去出库记录
function toOutboundRecord() {
  router.push({ name: 'WMSOutboundRecord' })
}

// 批量出库
function toBatchOutbound() {
  batchOutboundHandlingVisible.value = true
}

// 批量出库成功
function handleBatchOutbound() {
  updateListNumber()
  crud.toQuery()
}

// 批量调拨
function toBatchTransfer() {
  batchTransferHandlingVisible.value = true
}

// 批量调拨成功
function handleTransferOutbound() {
  crud.toQuery()
}

// 批量打印
function toBatchPrint() {}

// 打印 未打印标签的物料
function toPrintNotPrintedLabel() {}

// 打开冻结记录
function openFreezeRecords() {}

// 更新出库清单
function updateListNumber() {
  currentUserOutboundListRef.value && currentUserOutboundListRef.value.updateListNumber()
}

defineExpose({
  updateListNumber
})
</script>
