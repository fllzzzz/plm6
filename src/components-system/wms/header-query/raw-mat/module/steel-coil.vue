<template>
  <el-input
    v-model.trim="queryVO.specification"
    clearable
    style="width: 130px"
    size="small"
    placeholder="规格"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <el-input
    v-model.trim="queryVO.color"
    clearable
    style="width: 130px"
    size="small"
    placeholder="颜色"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <common-input-number
    v-model.trim="queryVO.thickness"
    mode="input"
    clearable
    style="width: 130px"
    size="small"
    placeholder="厚度"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <common-input-number
    v-model.trim="queryVO.width"
    mode="input"
    clearable
    style="width: 130px"
    size="small"
    placeholder="宽度"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <common-input-number
    v-model.trim="queryVO.minLength"
    mode="input"
    clearable
    style="width: 130px"
    size="small"
    placeholder="最小长度"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <common-input-number
    v-model.trim="queryVO.maxLength"
    mode="input"
    clearable
    style="width: 130px"
    size="small"
    placeholder="最大长度"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <el-input
    v-model.trim="queryVO.brand"
    clearable
    style="width: 130px"
    size="small"
    placeholder="品牌"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <!-- <el-input
    v-model.trim="queryVO.heatNoAndBatchNo"
    clearable
    style="width: 200px"
    size="small"
    placeholder="卷号"
    class="filter-item"
    @keyup.enter="toQuery"
  /> -->
  <batch-input class="filter-item" style="width: 200px" placeholder="卷号" :query="props.query" @to-query="toQuery" />
</template>

<script setup>
import { defineProps, defineEmits, ref, watchEffect } from 'vue'

import BatchInput from './batch-input.vue'

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
