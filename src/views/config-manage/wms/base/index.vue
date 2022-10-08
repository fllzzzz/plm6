<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <!-- 入库基础 -->
        <!-- <basic-inbound v-permission="permission.basicInboundGet" class="card" /> -->
        <!-- 钢板入库 -->
        <steel-inbound v-permission="permission.steelInboundGet" class="card" />
        <!-- 报表中心 -->
        <report-center v-permission="permission.reportCenterGet" class="card" />
      </el-col>
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <!-- 出库基础-->
        <basic-outbound v-permission="permission.steelInboundGet" class="card" />
        <!-- 退货基础 -->
        <!-- <basic-reject v-permission="permission.basicRejectGet" class="card" /> -->
      </el-col>
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <!-- 甲供归还 -->
        <party-a-borrow-return v-permission="permission.partyABorrowReturnGet" class="card" />
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { configWmsBasicForPM as permission } from '@/page-permission/config'
import { provide } from 'vue'

// import BasicInbound from './module/basic-inbound.vue'
import SteelInbound from './module/steel-inbound.vue'
import BasicOutbound from './module/basic-outbound.vue'
import ReportCenter from './module/report-center.vue'
// import BasicReject from './module/basic-reject.vue'
import PartyABorrowReturn from './module/party-a-borrow-return.vue'

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
