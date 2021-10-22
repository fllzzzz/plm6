<template>
  <div class="app-container">
    <basic-inbound v-permission="permission.basicInboundGet" class="card" />
    <steel-inbound v-permission="permission.steelInboundGet" class="card" />
  </div>
</template>

<script setup>
import basicInbound from './module/basic-inbound.vue'
import steelInbound from './module/steel-inbound.vue'

import { provide } from 'vue'

const permission = {
  basicInboundGet: ['config_wms_basicInbound:get'],
  basicInboundEdit: ['config_wms_basicInbound:edit'],
  steelInboundGet: ['config_wms_steelInbound:get'],
  steelInboundEdit: ['config_wms_steelInbound:edit']
}

provide('permission', permission)
</script>

<style lang="scss" scoped>
.card {
  width: 700px;
}

.card + .card {
  margin-top: 20px;
}
</style>
