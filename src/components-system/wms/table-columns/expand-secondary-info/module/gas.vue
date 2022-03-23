<template>
  <p v-if="showBrand" v-bind="$attrs">
    品牌：<span>{{ props.row.brand }}</span>
  </p>
  <p v-if="showRemark" v-bind="$attrs">
    备注：<span>{{ props.row.remark }}</span>
  </p>
  <slot v-bind="$attrs" />
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
