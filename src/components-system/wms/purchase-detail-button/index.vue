<template>
  <span class="purchase-detail-button">
    <common-button v-bind="$attrs" @click="openDetail" type="info">{{ props.btnName }}</common-button>
    <m-detail v-permission="permission" v-if="props.purchaseId" v-model="visible" :detail-id="props.purchaseId" />
  </span>
</template>

<script setup>
import { purchaseOrderDetailCPM as permission } from '@/page-permission/wms'

import { defineProps, ref } from 'vue'
import mDetail from '@/components-system/wms/purchase-sn-select/module/detail.vue'

const visible = ref(false)

const props = defineProps({
  purchaseId: {
    type: Number
  },
  btnName: {
    type: String,
    default: '采购单详情'
  }
})

function openDetail() {
  if (!props.purchaseId) return
  visible.value = true
}
</script>
