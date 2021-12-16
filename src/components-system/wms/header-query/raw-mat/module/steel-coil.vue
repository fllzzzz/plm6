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
  <el-input
    v-model.trim="queryVO.theoryThickness"
    clearable
    style="width: 130px"
    size="small"
    placeholder="理论厚度"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <el-input
    v-model.trim="queryVO.theoryThickness"
    clearable
    style="width: 130px"
    size="small"
    placeholder="宽度"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <el-input
    v-model.trim="queryVO.minLength"
    clearable
    style="width: 130px"
    size="small"
    placeholder="最小长度"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <el-input
    v-model.trim="queryVO.maxLength"
    clearable
    style="width: 130px"
    size="small"
    placeholder="最大长度"
    class="filter-item"
    @keyup.enter="toQuery"
  />
  <el-input
    v-model.trim="queryVO.heatNoAndBatchNo"
    clearable
    style="width: 200px"
    size="small"
    placeholder="卷号"
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
