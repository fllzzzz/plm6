<template>
  <el-tag v-if="detail.licensePlate" effect="plain">{{ `车牌：${detail.licensePlate}` }}</el-tag>
  <el-tag v-if="detail.shipmentNumber" effect="plain">{{ `物流单号：${detail.shipmentNumber}` }}</el-tag>
  <el-tag v-if="showLoadingWeight" effect="plain">
    {{ `过磅重量：${detail.loadingWeight} kg` }}
  </el-tag>
  <el-tag v-if="showFreight && isNotBlank(detail.freight)" effect="plain">{{ `运输费：${detail.freight} 元` }}</el-tag>
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
  <!-- <el-tag v-if="order.pickUpMode" v-parse-enum="{ e: pickUpModeEnum, v: order.pickUpMode }" type="info" effect="plain" /> -->
</template>

<script setup>
import { STEEL_ENUM } from '@/settings/config'
import { isNotBlank } from '@data-type/index'
import { weightMeasurementModeEnum } from '@/utils/enum/modules/finance'
import { logisticsPayerEnum } from '@/utils/enum/modules/logistics'
import { orderSupplyTypeEnum, purchaseOrderPaymentModeEnum } from '@/utils/enum/modules/wms'
import { computed, defineProps } from 'vue'

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

// 是否显示车的过磅重量
const showLoadingWeight = computed(
  () =>
    props.detail.basicClass & STEEL_ENUM &&
    props.order.weightMeasurementMode !== weightMeasurementModeEnum.THEORY.V &&
    props.detail.loadingWeight !== null &&
    props.detail.loadingWeight !== undefined
)

// 是否显示运费
const showFreight = computed(() => props.order.logisticsPayerType === logisticsPayerEnum.DEMAND.V)
</script>

<style lang="scss" scoped>
.el-tag {
  min-width: 70px;
  text-align: center;
}
</style>
