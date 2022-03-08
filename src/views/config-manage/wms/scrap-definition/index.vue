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
import { configWmsScrapDefinitionPM as permission } from '@/page-permission/config'
import { provide } from 'vue'
import steel from './module/steel.vue'

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
