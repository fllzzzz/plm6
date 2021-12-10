<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <steel v-permission="permission.steelScrapDefinitionGet" class="card" />
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import steel from './module/steel.vue'

import { provide } from 'vue'

const permission = {
  steelScrapDefinitionGet: ['config_wms_steelScrapDefinition:get'],
  steelScrapDefinitionEdit: ['config_wms_steelScrapDefinition:edit']
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
