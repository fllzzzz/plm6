<template>
  <div class="app-container">
    <el-row :gutter="20">
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <overweightSMSRecipient v-permission="permission.overweightSMSRecipientGet" style="margin-bottom: 20px" />
        <safeAmount v-permission="permission.safeAmountFactorGet" style="margin-bottom: 20px" />
        <steelBindConfig v-permission="permission.steelBindConfigGet" style="margin-bottom: 20px" />
        <price-manage-config v-permission="permission.machinePartStructureGet"/>
        <!-- <installationAudit v-permission="permission.installationAuditGet" style="margin-top: 20px" /> -->
        <!-- <driverFillConfig v-permission="permission.driverFillConfigGet" style="margin-bottom: 20px" /> -->
      </el-col>
      <el-col :xs="24" :sm="24" :md="12" :lg="8">
        <drawingSNConfig v-permission="permission.drawingSNConfigGet" style="margin-bottom: 20px" />
        <appPrintConfig v-permission="permission.appPrintConfigGet" style="margin-bottom: 20px" />
        <foldingPriceMethod
          v-if="productMenu & componentTypeEnum.ENCLOSURE.V"
          v-permission="permission.foldingPriceMethodGet"
          style="margin-bottom: 20px"
        />
        <app-task-report v-permission="permission.appTaskReportGet" style="margin-bottom: 20px" />
      </el-col>
      <el-col :xs="24" :sm="24" :md="24" :lg="8">
        <carModelConfig v-permission="permission.carModelConfigGet" style="margin-bottom: 20px" />
        <machine-part-scheduling-config v-permission="permission.machinePartSchedulingGet" style="margin-bottom: 20px" />
        <structure-audit-config v-permission="permission.contractPriceConfigGet"/>
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
// import driverFillConfig from './component/driver-fill-config'
import drawingSNConfig from './component/drawing-sn-config'
import appPrintConfig from './component/app-print-config'
import carModelConfig from './component/car-model-config'
import foldingPriceMethod from './component/folding-price-method'
import steelBindConfig from './component/steel-bind-config.vue'
import appTaskReport from './component/app-task-report.vue'
import machinePartSchedulingConfig from './component/machine-part-scheduling-config.vue'
import structureAuditConfig from './component/structure-audit-config'
import priceManageConfig from './component/price-manage-config'

provide('permission', permission)

const { productMenu } = mapGetters('productMenu')
</script>
