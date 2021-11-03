<template>
  <template v-if="change">
    <span v-if="isNotBlank(props.old)" style="color: red">{{ oldValue }}</span>
    <span v-else style="color: red">未填写</span>
    ▶
    <span v-if="isNotBlank(props.new)" style="color: green">{{ newValue }}</span>
    <span v-else style="color: green">未填写</span>
  </template>
  <span v-else>/</span>
</template>

<script setup>
import { computed, defineProps } from 'vue'
import { isNotBlank } from '@data-type'

const props = defineProps({
  old: null,
  new: null,
  enum: Object
})

const change = computed(() => props.old !== props.new)

const oldValue = computed(() => {
  if (props.enum) {
    return props.enum.VL[props.old]
  }
  return props.old
})

const newValue = computed(() => {
  if (props.enum) {
    return props.enum.VL[props.new]
  }
  return props.new
})
</script>
