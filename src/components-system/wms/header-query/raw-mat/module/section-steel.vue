<template>
  <el-input
    v-model.trim="queryVO.specification"
    clearable
    style="width: 200px"
    size="small"
    placeholder="规格"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <el-input
    v-model.trim="queryVO.length"
    clearable
    style="width: 200px"
    size="small"
    placeholder="长度"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <el-input
    v-model.trim="queryVO.heatNoAndBatchNo"
    clearable
    style="width: 200px"
    size="small"
    placeholder="炉批号"
    class="filter-item"
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
