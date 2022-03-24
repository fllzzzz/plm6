<template>
  <p v-if="showBrand">
    品牌：<span>{{ props.row.brand }}</span>
  </p>
  <p v-if="showRemark">
    备注：<span>{{ props.row.remark }}</span>
  </p>
  <slot />
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
  },
  showBrand: {
    type: Boolean,
    default: false
  }
})
</script>
