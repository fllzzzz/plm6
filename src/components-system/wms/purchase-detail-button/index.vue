<template>
  <span v-if="checkPermission(permission)" class="purchase-detail-button">
    <common-button v-bind="$attrs" @click="openDetail(props.purchaseId)" type="info">{{ props.btnName }}</common-button>
    <!-- 采购合同详情 -->
    <detail-wrapper ref="purchaseOrderRef" :api="getPurchaseOrderDetail">
      <purchase-order-detail />
    </detail-wrapper>
  </span>
</template>

<script setup>
import { detail as getPurchaseOrderDetail } from '@/api/supply-chain/purchase-order'
import { purchaseOrderDetailCPM as permission } from '@/page-permission/supply-chain'

import { defineProps } from 'vue'
import useOtherCrudDetail from '@/composables/use-other-crud-detail'
import DetailWrapper from '@crud/detail-wrapper.vue'
import PurchaseOrderDetail from '@/views/supply-chain/purchase-order/module/detail/raw-material.vue'
import checkPermission from '@/utils/system/check-permission'

const props = defineProps({
  purchaseId: {
    type: Number
  },
  btnName: {
    type: String,
    default: '采购合同详情'
  }
})

// 采购合同详情
const { detailRef: purchaseOrderRef, openDetail } = useOtherCrudDetail()

</script>
