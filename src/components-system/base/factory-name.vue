<template>
  <span>{{ name }}</span>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import useFactory from '@compos/store/use-factories'

const props = defineProps({
  factoryId: {
    type: Number
  }
})

const { factoryKV } = useFactory()

const name = computed(() => {
  if (props.factoryId) {
    return factoryKV.value[props.factoryId]
  } else {
    return ''
  }
})

</script>
