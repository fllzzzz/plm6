<template>
  <span class="factory-table-cell-tag" :style="{'margin-left': `${props.offset}px`}">
    <table-cell-tag v-if="cur" :name="cur.shortName" :color="cur.tagColor" />
  </span>
</template>

<script setup>
import { defineProps, computed } from 'vue'

import useFactory from '@compos/store/use-factories'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'

const props = defineProps({
  id: {
    type: Number
  },
  offset: {
    type: Number,
    default: 15
  }
})

const { factoryKV } = useFactory()

const cur = computed(() => {
  return factoryKV.value[props.id]
})

</script>
