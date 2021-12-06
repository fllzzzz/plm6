<template>
  <p>
    品牌：<span v-empty-text>{{ props.row.brand }}</span>
  </p>
  <p>
    炉批号/卷号：<span v-empty-text>{{ props.row.heatNoAndBatchNo }}</span>
  </p>
  <p v-if="showRemark">
    备注：<span v-empty-text>{{ props.row.remark }}</span>
  </p>
</template>

<script setup>
import { defineProps } from 'vue'

const props = defineProps({
  row: {
    type: Object,
    default: () => {
      return {}
    }
  },
  showRemark: {
    type: Boolean,
    default: false
  }
})
</script>
