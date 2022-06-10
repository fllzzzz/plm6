<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :xs="24" :sm="24" :md="12" :lg="10">
        <overweightSMSRecipient v-permission="permission.overweightSMSRecipientGet" />
        <safeAmount v-permission="permission.safeAmountFactorGet" style="margin-top: 20px" />
        <!-- <installationAudit v-permission="permission.installationAuditGet" style="margin-top: 20px" /> -->
        <driverFillConfig v-permission="permission.driverFillConfigGet" style="margin-top: 20px" />
      </el-col>
      <el-col :xs="24" :sm="24" :md="12" :lg="10">
        <appPrintConfig  v-permission="permission.appPrintConfigGet"/>
        <foldingPriceMethod v-if="productMenu & componentTypeEnum.ENCLOSURE.V"  v-permission="permission.foldingPriceMethodGet" style="margin-top: 20px"/>
        <carModelConfig  v-permission="permission.carModelConfigGet" style="margin-top: 20px"/>
      </el-col>
    </el-row>
  </div>
</template>

<script setup>
import { provide } from 'vue'
import { configMesBasePM as permission } from '@/page-permission/config'
import { mapGetters } from '@/store/lib'
import { componentTypeEnum } from '@enum-ms/mes'

import overweightSMSRecipient from './component/overweight-sms-config'
import safeAmount from './component/safe-amount-config'
// import installationAudit from './component/installation-audit'
import driverFillConfig from './component/driver-fill-config'
import appPrintConfig from './component/app-print-config'
import carModelConfig from './component/car-model-config'
import foldingPriceMethod from './component/folding-price-method'

provide('permission', permission)

const { productMenu } = mapGetters('productMenu')
</script>
