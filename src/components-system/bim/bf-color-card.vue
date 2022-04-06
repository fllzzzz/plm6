<template>
  <color-card :colors="colors" v-model:value="selectStatus" color-border select-able @change="statusChange"></color-card>
</template>

<script setup>
import ColorCard from '@/components/ColorCard'
import { defineProps, ref } from 'vue'

const selectStatus = ref()

defineProps({
  statusChange: {
    type: Function
  },
  colors: {
    type: Array,
    default: () => []
  }
})
</script>
