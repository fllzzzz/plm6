<template>
  <span>
    <template v-if="props.transferReceipt.transferType !== transferTypeEnum.RETURN_PARTY_A.V && props.transferReceipt.direction">
      <span class="project-ware-text" v-if="props.transferReceipt.direction.project">
        {{ props.transferReceipt.direction.project }}
      </span>
      <span class="public-ware-text" v-else>公共库</span>
      <span v-if="props.transferReceipt.direction.workshop">（{{ props.transferReceipt.direction.workshop.name }}）</span>
    </template>
    <span v-else>-</span>
  </span>
</template>

<script setup>
import { defineProps } from 'vue'
import { transferTypeEnum } from '@/utils/enum/modules/wms'

const props = defineProps({
  transferReceipt: {
    type: Object,
    default: () => {
      return {}
    }
  }
})
</script>
