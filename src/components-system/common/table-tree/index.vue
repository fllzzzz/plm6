<template>
  <div class="table-tree">
    <table-item v-for="item in props.dataSource" :key="item.key" :item="item" :keys="keys" @on-click="onClick" />
  </div>
</template>

<script setup>
import { defineEmits, defineProps, ref } from 'vue'
import TableItem from './TableItem.vue'
const props = defineProps({
  dataSource: {
    type: Array,
    default: () => []
  },
  keys: {
    type: Array,
    default: () => []
  }
})
const emit = defineEmits(['change'])
const selects = ref(new Set([...props.keys]))
function onClick(state, value) {
  if (state) {
    selects.value.add(value)
  } else {
    selects.value.delete(value)
  }
  emit('change', selects.value.values())
}
</script>

<style scoped>
  .table-tree{border: 1px solid #e5e5e5; border-bottom: 0;}
</style>
