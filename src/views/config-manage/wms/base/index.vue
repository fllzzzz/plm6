<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <basic-inbound v-permission="permission.basicInboundGet" class="card" />
        <steel-inbound v-permission="permission.steelInboundGet" class="card" />
      </el-col>
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <basic-outbound v-permission="permission.steelInboundGet" class="card" />
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import basicInbound from './module/basic-inbound.vue'
import steelInbound from './module/steel-inbound.vue'
import basicOutbound from './module/basic-outbound.vue'

import { provide } from 'vue'

const permission = {
  basicInboundGet: ['config_wms_basicInbound:get'],
  basicInboundEdit: ['config_wms_basicInbound:edit'],
  steelInboundGet: ['config_wms_steelInbound:get'],
  steelInboundEdit: ['config_wms_steelInbound:edit'],
  basicOutboundGet: ['config_wms_basicOutbound:get'],
  basicOutboundEdit: ['config_wms_basicOutbound:edit']
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
