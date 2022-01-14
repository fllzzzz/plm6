<template>
  <span>
    <template v-if="props.transferReceipt.transferType !== transferTypeEnum.RETURN_PARTY_A.V && props.transferReceipt.direction">
      <span
        class="project-ware-text"
        v-if="props.transferReceipt.direction.project"
        v-parse-project="{ project: props.transferReceipt.direction.project, onlyShortName: true }"
        v-empty-text
      />
      <span class="public-ware-text" v-else>公共库</span>
      <span v-if="props.transferReceipt.direction.factory">（{{ props.transferReceipt.direction.factory.name }}）</span>
    </template>
    <span v-else v-empty-text />
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
