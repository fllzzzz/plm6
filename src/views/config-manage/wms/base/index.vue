<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <!-- 入库基础 -->
        <basic-inbound v-if="checkPermission(permission.basicInboundGet)" class="card" />
        <!-- 采购基础 -->
        <basic-purchase v-if="checkPermission(permission.basicPurchaseGet)" class="card" />
        <!-- 钢板入库 -->
        <!-- <steel-inbound v-if="checkPermission(permission.steelInboundGet)" class="card" /> -->
      </el-col>
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <!-- 出库基础-->
        <basic-outbound v-if="checkPermission(permission.basicOutboundGet)" class="card" />
        <!-- 物料加权 -->
        <material-weighting v-if="checkPermission(permission.materialWeightingGet)" class="card" />
        <!-- 退货基础 -->
        <!-- <basic-reject v-if="checkPermission(permission.basicRejectGet)" class="card" /> -->
      </el-col>
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <!-- 甲供归还 -->
        <party-a-borrow-return v-if="checkPermission(permission.partyABorrowReturnGet)" class="card" />
        <!-- 报表中心 -->
        <report-center v-if="checkPermission(permission.reportCenterGet)" class="card" />
         <!-- 退库审核 -->
        <basic-return v-if="checkPermission(permission.basicReturnGet)" class="card" />
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { configWmsBasicForPM as permission } from '@/page-permission/config'
import { provide } from 'vue'
import checkPermission from '@/utils/system/check-permission'

import BasicInbound from './module/basic-inbound.vue'
import BasicPurchase from './module/basic-purchase.vue'
// import SteelInbound from './module/steel-inbound.vue'
import BasicOutbound from './module/basic-outbound.vue'
import ReportCenter from './module/report-center.vue'
// import BasicReject from './module/basic-reject.vue'
import PartyABorrowReturn from './module/party-a-borrow-return.vue'
import MaterialWeighting from './module/material-weighting.vue'
import basicReturn from './module/basic-return.vue'

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
