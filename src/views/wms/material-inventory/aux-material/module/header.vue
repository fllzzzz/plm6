<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <!-- 物料查询相关 -->
      <mat-header-query :basic-class="query.basicClass" :query="query" :to-query="crud.toQuery" show-project-Warehouse-type>
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
            @change="crud.toQuery"
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
        <current-user-outbound-list ref="currentUserOutboundListRef" @refresh="crud.toQuery" />
        <common-button class="filter-item" icon="el-icon-time" size="mini" type="info" @click="toOutboundRecord">出库记录</common-button>
        <common-button class="filter-item" type="info" size="mini" icon="el-icon-lock" @click="openFreezeRecords">
          冻结记录
        </common-button>
      </template>
    </crud-operation>
    <outbound-batch-handling-form
      v-model:visible="batchOutboundHandlingVisible"
      :project-warehouse-type="query.projectWarehouseType"
      :project-id="query.projectId"
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
import { defineExpose } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'

import useHeaderInfo from '../../compos/use-header-info'
import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'
import CurrentUserOutboundList from '@/views/wms/material-outbound/raw-material/components/current-user-outbound-list/index.vue'
import OutboundBatchHandlingForm from '@/views/wms/material-outbound/raw-material/components/outbound-batch-handling-form/index.vue'
import TransferBatchHandlingForm from '@/views/wms/material-transfer/raw-material/components/transfer-batch-handling-form/index.vue'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'

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
} = useHeaderInfo({ defaultBasicClass: rawMatClsEnum.MATERIAL.V })

defineExpose({
  updateListNumber
})
</script>
