<template>
  <template v-if="receipts.length > 0">
    <template v-for="(receipt, rIndex) in receipts" :key="`${receipt.serialNumber}_${rIndex}`">
      <!-- 当前页面出库申请单无法查看详情（出库申请单会被清空） -->
      <span v-bind="$attrs" v-if="receiptType === receiptTypeEnum.OUTBOUND_APPLY.V">
        {{ receipt.serialNumber }}
      </span>
      <clickable-permission-span
        v-bind="$attrs"
        v-else
        :permission="openDetailPermission(receiptType)"
        @click="openReceiptDetail(receipt.id, receiptType)"
        :text="receipt.serialNumber"
      />

      <span v-if="rIndex !== receipts.length - 1"> 、</span>
    </template>
  </template>
  <span v-else v-bind="$attrs">-</span>
  <receipt-detail ref="receiptDetailRef" :receipt-types="props.receiptTypes" />
</template>

<script setup>
import { receiptDetailCPM as permission } from '@/page-permission/wms'
import { ref, defineProps, computed } from 'vue'
import { receiptTypeEnum } from '@/utils/enum/modules/wms'
import ReceiptDetail from '@/components-system/wms/receipt-detail'
import ClickablePermissionSpan from '@/components-system/common/clickable-permission-span.vue'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
  receipt: {
    type: [Array, Object],
    default: () => ({})
  },
  receiptTypes: {
    type: Array,
    default: () => []
  }
})

// 单据详情
const receiptDetailRef = ref()

const receipts = computed(() => {
  if (isBlank(props.receipt) && typeof props.receipt === 'object') return []
  if (Array.isArray(props.receipt)) {
    return props.receipt
  }
  return [props.receipt]
})

const receiptType = computed(() => {
  if (props.receipt.receiptType) return props.receipt.receiptType
  if (!props.receipt.receiptType && props.receiptTypes && props.receiptTypes.length === 1) {
    return receiptTypeEnum[props.receiptTypes[0]].V
  }
  return undefined
})

// 打开详情
function openReceiptDetail(detailId, type) {
  receiptDetailRef.value && receiptDetailRef.value.openDetail(detailId, type)
}

// 查看详情权限
function openDetailPermission(type) {
  switch (type) {
    case receiptTypeEnum.REQUISITIONS.V:
      return permission.requisitionsDetail
    case receiptTypeEnum.PURCHASE.V:
      return permission.purchaseOrderDetail
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
    case receiptTypeEnum.SUPPLEMENT.V:
      return permission.supplementReceiptDetail
    default:
      return
  }
}
</script>
