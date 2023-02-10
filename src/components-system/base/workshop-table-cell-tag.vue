<template>
  <span class="workshop-table-cell-tag" :style="{'margin-left': `${props.offset}px`}">
    <table-cell-tag v-if="cur" :name="cur.name" :color="cur.tagColor" />
  </span>
</template>

<script setup>
import { defineProps, computed } from 'vue'

import useWorkshop from '@compos/store/use-workshops'
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

const { workshopKV } = useWorkshop()

const cur = computed(() => {
  return workshopKV.value[props.id]
})

</script>
