<template>
  <template v-if="props.value">
    <span v-if="props.value > 0" style="color: #d00000">
      <svg-icon icon-class="comp-compare-down" />
      <span style="font-weight: 600">{{ props.value }}</span>
    </span>
    <span v-else style="color: #00d064">
      <svg-icon icon-class="comp-compare-down" style="transform: rotate(180deg)" />
      <span style="font-weight: 600">{{ -props.value }}</span>
    </span>
  </template>
  <span v-else>/</span>
</template>

<script setup>
import { defineProps } from 'vue'

const props = defineProps({
  value: null
})
</script>
