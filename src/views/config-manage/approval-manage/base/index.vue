<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :xs="24" :sm="24" :md="12" :lg="10">
        <approval-config v-if="checkPermission(permission.approvalConfigGet)" />
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { provide } from 'vue'
import { configProcessBasicPM as permission } from '@/page-permission/config'
import approvalConfig from './component/approval-config'
import checkPermission from '@/utils/system/check-permission'

provide('permission', permission)
</script>
