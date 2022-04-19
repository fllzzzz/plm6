<template>
  <color-card :colors="colors" v-model:value="selectStatus" color-border :select-able="selectAble" @change="statusChange"></color-card>
</template>

<script setup>
import ColorCard from '@/components/ColorCard'
import { defineProps, ref } from 'vue'

const selectStatus = ref()

defineProps({
  statusChange: {
    type: Function
  },
  selectAble: {
    type: Boolean,
    default: true
  },
  colors: {
    type: Array,
    default: () => []
  }
})
</script>
