<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <!-- 入库基础 -->
        <basic-inbound v-permission="permission.basicInboundGet" class="card" />
        <!-- 钢板入库 -->
        <steel-inbound v-permission="permission.steelInboundGet" class="card" />
      </el-col>
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <!-- 出库基础-->
        <basic-outbound v-permission="permission.steelInboundGet" class="card" />
        <!-- 退货基础 -->
        <basic-reject v-permission="permission.basicRejectGet" class="card" />
      </el-col>
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <!-- 甲供归还 -->
        <party-a-borrow-return v-permission="permission.partyABorrowReturnGet" class="card" />
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import BasicInbound from './module/basic-inbound.vue'
import SteelInbound from './module/steel-inbound.vue'
import BasicOutbound from './module/basic-outbound.vue'
import BasicReject from './module/basic-reject.vue'
import PartyABorrowReturn from './module/party-a-borrow-return.vue'
import { provide } from 'vue'

const permission = {
  basicInboundGet: ['config_wms_basicInbound:get'],
  basicInboundEdit: ['config_wms_basicInbound:edit'],
  steelInboundGet: ['config_wms_steelInbound:get'],
  steelInboundEdit: ['config_wms_steelInbound:edit'],
  basicOutboundGet: ['config_wms_basicOutbound:get'],
  basicOutboundEdit: ['config_wms_basicOutbound:edit'],
  partyABorrowReturnGet: ['config_wms_partyABorrowReturn:get'],
  partyABorrowReturnEdit: ['config_wms_partyABorrowReturn:edit'],
  basicRejectGet: ['config_wms_basicReject:get'],
  basicRejectEdit: ['config_wms_basicReject:edit']
}

provide('permission', permission)
</script>

<style lang="scss" scoped>
.app-container {
  padding-top: 0;
}

.card {
  margin-top: 20px;
}
</style>
