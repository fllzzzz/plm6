<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :xs="24" :sm="24" :md="12" :lg="10">
        <overweightSMSRecipient v-permission="permission.overweightSMSRecipientGet" />
        <!-- <safeAmount v-permission="permission.safeAmountFactorGet" style="margin-top: 20px" /> -->
        <!-- <installationAudit v-permission="permission.installationAuditGet" style="margin-top: 20px" /> -->
        <driverFillConfig v-permission="permission.driverFillConfigGet" style="margin-top: 20px" />
      </el-col>
      <el-col :xs="24" :sm="24" :md="12" :lg="10">
        <carModelConfig  v-permission="permission.carModelConfigGet"/>
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { provide } from 'vue'
import overweightSMSRecipient from './component/overweightSMSRecipient'
// import safeAmount from './component/safeAmount'
// import installationAudit from './component/installation-audit'
import driverFillConfig from './component/driver-fill-config'
import carModelConfig from './component/car-model-config'

const permission = {
  overweightSMSRecipientGet: ['overweightSMSRecipient:get'],
  overweightSMSRecipientEdit: ['overweightSMSRecipient:edit'],
  safeAmountFactorGet: ['safeAmountFactor:get'],
  installationAuditGet: ['installationAudit:get'],
  driverFillConfigGet: ['driverFillConfig:get'],
  driverFillConfigEdit: ['driverFillConfig:edit'],
  carModelConfigGet: ['carModelConfig:get'],
  carModelConfigEdit: ['carModelConfig:edit']
}

provide('permission', permission)
</script>
