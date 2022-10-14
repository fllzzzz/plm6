<template>
  <!-- 采购合同详情 -->
  <detail-wrapper v-if="receiptExist[receiptTypeEnum.PURCHASE.K]" ref="purchaseOrderRef" :api="getPurchaseOrderDetail">
    <purchase-order-detail />
  </detail-wrapper>

  <!-- 入库单详情 -->
  <detail-wrapper v-if="receiptExist[receiptTypeEnum.INBOUND.K]" ref="inboundDetailRef" :api="getInboundDetail">
    <inbound-detail />
  </detail-wrapper>

  <!-- 出库详情 -->
  <detail-wrapper v-if="receiptExist[receiptTypeEnum.OUTBOUND.K]" ref="outboundDetailRef" :api="getOutboundDetail">
    <outbound-detail />
  </detail-wrapper>

  <!-- 调拨详情 -->
  <detail-wrapper v-if="receiptExist[receiptTypeEnum.TRANSFER.K]" ref="transferDetailRef" :api="getTransferDetail">
    <transfer-detail />
  </detail-wrapper>

  <!-- 退库单详情 -->
  <detail-wrapper v-if="receiptExist[receiptTypeEnum.RETURN.K]" ref="returnDetailRef" :api="getReturnDetail">
    <return-detail />
  </detail-wrapper>

  <!-- 退货详情 -->
  <detail-wrapper v-if="receiptExist[receiptTypeEnum.REJECTED.K]" ref="rejectDetailRef" :api="getRejectDetail">
    <reject-detail />
  </detail-wrapper>

  <!-- 红冲详情 -->
  <detail-wrapper v-if="receiptExist[receiptTypeEnum.SUPPLEMENT.K]" ref="supplementDetailRef" :api="getSupplementDetail">
    <supplement-detail />
  </detail-wrapper>
</template>

<script setup>
import { ref, watch, defineProps, defineExpose } from 'vue'
import { receiptTypeEnum } from '@/utils/enum/modules/wms'

import { detail as getPurchaseOrderDetail } from '@/api/supply-chain/purchase-order'
import { detail as getInboundDetail } from '@/api/wms/material-inbound/raw-material/review'
import { detail as getOutboundDetail } from '@/api/wms/material-outbound/raw-material/record'
import { detail as getTransferDetail } from '@/api/wms/material-transfer/raw-material/review'
import { detail as getReturnDetail } from '@/api/wms/material-return/raw-material/record'
import { detail as getRejectDetail } from '@/api/wms/material-reject/raw-material/review'
import { detail as getSupplementDetail } from '@/api/wms/report/raw-material/supplement'

import useOtherCrudDetail from '@compos/use-other-crud-detail'
import DetailWrapper from '@crud/detail-wrapper.vue'
import purchaseOrderDetail from '@/views/supply-chain/purchase-order/module/detail/raw-material.vue'
import InboundDetail from '@/views/wms/material-inbound/raw-material/review/module/detail.vue'
import OutboundDetail from '@/views/wms/material-outbound/raw-material/record/module/detail.vue'
import TransferDetail from '@/views/wms/material-transfer/raw-material/review/module/detail.vue'
import ReturnDetail from '@/views/wms/material-return/raw-material/record/module/detail.vue'
import RejectDetail from '@/views/wms/material-reject/raw-material/review/module/detail.vue'
import SupplementDetail from '@/views/wms/report/raw-material/supplement-record/module/detail.vue'

const props = defineProps({
  receiptTypes: {
    type: Array,
    default: () => []
  }
})

// 是否存在
const receiptExist = ref({})

watch(
  () => props.receiptTypes,
  (newVal) => {
    setReceiptExist(newVal)
  },
  { immediate: true }
)

// 采购合同详情
const { detailRef: purchaseOrderRef, openDetail: openPurchaseDetail } = useOtherCrudDetail()
// 入库单详情
const { detailRef: inboundDetailRef, openDetail: openInboundDetail } = useOtherCrudDetail()
// 出库单详情
const { detailRef: outboundDetailRef, openDetail: openOutboundDetail } = useOtherCrudDetail()
// 调拨单详情
const { detailRef: transferDetailRef, openDetail: openTransferDetail } = useOtherCrudDetail()
// 退库单详情
const { detailRef: returnDetailRef, openDetail: openReturnDetail } = useOtherCrudDetail()
// 退货单详情
const { detailRef: rejectDetailRef, openDetail: openRejectDetail } = useOtherCrudDetail()
// 红冲单详情
const { detailRef: supplementDetailRef, openDetail: openSupplementDetail } = useOtherCrudDetail()

function setReceiptExist(receiptTypes = []) {
  const re = {
    [receiptTypeEnum.PURCHASE.K]: false,
    [receiptTypeEnum.INBOUND.K]: false,
    [receiptTypeEnum.OUTBOUND.K]: false,
    [receiptTypeEnum.OUTBOUND_APPLY.K]: false,
    [receiptTypeEnum.TRANSFER.K]: false,
    [receiptTypeEnum.RETURN.K]: false,
    [receiptTypeEnum.REJECTED.K]: false,
    [receiptTypeEnum.SUPPLEMENT.K]: false
  }
  receiptTypes.forEach((type) => {
    re[type] = true
  })
  receiptExist.value = re
}

// 打开详情
function openDetail(detailId, receiptType) {
  if (!receiptType && props.receiptTypes && props.receiptTypes.length === 1) {
    receiptType = receiptTypeEnum[props.receiptTypes[0]].V
  }
  switch (receiptType) {
    case receiptTypeEnum.PURCHASE.V:
      openPurchaseDetail(detailId)
      return
    case receiptTypeEnum.INBOUND.V:
      openInboundDetail(detailId)
      return
    case receiptTypeEnum.OUTBOUND.V:
      openOutboundDetail(detailId)
      return
    case receiptTypeEnum.TRANSFER.V:
      openTransferDetail(detailId)
      return
    case receiptTypeEnum.RETURN.V:
      openReturnDetail(detailId)
      return
    case receiptTypeEnum.REJECTED.V:
      openRejectDetail(detailId)
      return
    case receiptTypeEnum.SUPPLEMENT.V:
      openSupplementDetail(detailId)
      return
    case receiptTypeEnum.OUTBOUND_APPLY.V:
      return
    default:
      return
  }
}

defineExpose({
  openDetail
})
</script>
