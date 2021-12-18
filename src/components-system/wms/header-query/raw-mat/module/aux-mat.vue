<template>
  <el-input
    v-model.trim="queryVO.specification"
    clearable
    size="small"
    placeholder="规格"
    class="filter-item"
    style="width: 250px"
    @keyup.enter="toQuery"
  />
  <el-input
    v-model.trim="queryVO.color"
    clearable
    size="small"
    placeholder="颜色"
    class="filter-item"
    style="width: 150px"
    @keyup.enter="toQuery"
  />
</template>

<script setup>
import { defineProps, defineEmits, ref, watchEffect } from 'vue'

const emit = defineEmits(['to-query'])

const props = defineProps({
  basicClass: {
    type: Number
  },
  query: {
    type: Object,
    default: () => {
      return {}
    }
  }
})

const queryVO = ref({})
watchEffect(() => {
  queryVO.value = props.query
})

function toQuery() {
  emit('to-query')
}
</script>
