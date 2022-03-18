<template>
  <!-- 当前页面出库申请单无法查看详情（出库申请单会被清空） -->
  <span v-bind="$attrs" v-if="props.receipt.receiptType === receiptTypeEnum.OUTBOUND_APPLY.V">
    {{ props.receipt.serialNumber }}
  </span>
  <clickable-permission-span
    v-bind="$attrs"
    v-else
    :permission="openDetailPermission(props.receipt.receiptType)"
    @click="openReceiptDetail(props.receipt.id, props.receipt.receiptType)"
    :text="props.receipt.serialNumber"
  />
  <receipt-detail ref="receiptDetailRef" :receipt-types="props.receiptTypes" />
</template>

<script setup>
import { receiptDetailCPM as permission } from '@/page-permission/wms'
import { ref, defineProps } from 'vue'
import { receiptTypeEnum } from '@/utils/enum/modules/wms'
import ReceiptDetail from '@/components-system/wms/receipt-detail'
import ClickablePermissionSpan from '@/components-system/common/clickable-permission-span.vue'

const props = defineProps({
  receipt: {
    type: Object,
    default: () => ({})
  },
  receiptTypes: {
    type: Array,
    default: () => []
  }
})

// 单据详情
const receiptDetailRef = ref()

// 打开详情
function openReceiptDetail(detailId, type) {
  receiptDetailRef.value && receiptDetailRef.value.openDetail(detailId, type)
}

// 查看详情权限
function openDetailPermission(type) {
  switch (type) {
    case receiptTypeEnum.PURCHASE.V:
      return permission.preparationReceiptDetail
    case receiptTypeEnum.INBOUND.V:
      return permission.inboundReceiptDetail
    case receiptTypeEnum.OUTBOUND.V:
      return permission.outboundReceiptDetail
    case receiptTypeEnum.TRANSFER.V:
      return permission.transferReceiptDetail
    case receiptTypeEnum.RETURN.V:
      return permission.returnReceiptDetail
    case receiptTypeEnum.REJECTED.V:
      return permission.rejectReceiptDetail
    default:
      return
  }
}
</script>
