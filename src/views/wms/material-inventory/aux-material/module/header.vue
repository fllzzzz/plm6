<template>
  <div class="head-container">
    <div v-if="crud.searchToggle">
      <!-- 物料查询相关 -->
      <mat-header-query :basic-class="query.basicClass" :query="query" :to-query="crud.toQuery" show-project-Warehouse-type>
        <template #firstLineRight>
          <span class="child-mr-6">
            <current-user-outbound-list ref="currentUserOutboundListRef" />
            <common-button icon="el-icon-time" size="mini" type="info" @click="toOutboundRecord">出库记录</common-button>
            <common-button v-permission="permission.freezeList" type="info" size="mini" icon="el-icon-lock" @click="openFreezeRecords">
              冻结记录
            </common-button>
          </span>
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
import { defineExpose } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import useHeaderInfo from '../../compos/use-header-info'
import RrOperation from '@crud/RR.operation'
import CrudOperation from '@crud/CRUD.operation'
import MatHeaderQuery from '@/components-system/wms/header-query/raw-mat/index.vue'
import CurrentUserOutboundList from '@/views/wms/outbound-components/current-user-outbound-list/index.vue'
import OutboundBatchHandlingForm from '@/views/wms/outbound-components/outbound-batch-handling-form/index.vue'
import TransferBatchHandlingForm from '@/views/wms/transfer-components/transfer-batch-handling-form/index.vue'

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
