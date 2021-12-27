<template>
  <el-tag effect="plain">{{ `车牌：${detail.licensePlate}` }}</el-tag>
  <el-tag v-if="detail.basicClass & STEEL_ENUM && order.weightMeasurementMode !== weightMeasurementModeEnum.THEORY.V" effect="plain">
    {{ `过磅重量：${detail.loadingWeight}kg` }}
  </el-tag>
  <el-tag v-if="order.supplyType" v-parse-enum="{ e: orderSupplyTypeEnum, v: order.supplyType }" type="info" effect="plain" />
  <el-tag
    v-if="order.weightMeasurementMode"
    v-parse-enum="{ e: weightMeasurementModeEnum, v: order.weightMeasurementMode }"
    type="info"
    effect="plain"
  />
  <el-tag
    v-if="order.purchaseOrderPaymentMode"
    v-parse-enum="{ e: purchaseOrderPaymentModeEnum, v: order.purchaseOrderPaymentMode }"
    type="info"
    effect="plain"
  />
  <el-tag v-if="order.pickUpMode" v-parse-enum="{ e: pickUpModeEnum, v: order.pickUpMode }" type="info" effect="plain" />
</template>

<script setup>
import { STEEL_ENUM } from '@/settings/config'
import { weightMeasurementModeEnum } from '@/utils/enum/modules/finance'
import { orderSupplyTypeEnum, pickUpModeEnum, purchaseOrderPaymentModeEnum } from '@/utils/enum/modules/wms'
import { defineProps } from 'vue'

// eslint-disable-next-line no-unused-vars
const props = defineProps({
  order: {
    type: Object,
    default: () => {
      return {}
    }
  },
  detail: {
    type: Object,
    default: () => {
      return {}
    }
  }
})
</script>

<style lang="scss" scoped>
.el-tag {
  min-width: 70px;
  text-align: center;
}
</style>
