<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :xs="24" :sm="24" :md="12" :lg="10">
        <problemConfig/>
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
// import { provide } from 'vue'
// import { configMesBasePM as permission } from '@/page-permission/config'

import problemConfig from './component/problem-config/index'

// provide('permission', permission)
</script>
