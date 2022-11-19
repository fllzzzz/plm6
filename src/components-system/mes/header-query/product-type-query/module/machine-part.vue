<template>
  <el-input
    v-model.trim="queryVO.serialNumber"
    size="small"
    placeholder="输入编号搜索"
    style="width: 170px"
    class="filter-item"
    clearable
    @keyup.enter="toQuery"
  />
</template>

<script setup>
import { defineProps, defineEmits, ref, watchEffect } from 'vue'

const emit = defineEmits(['to-query'])

const props = defineProps({
  productType: {
    type: Number
  },
  query: {
    type: Object,
    default: () => {
      return {}
    }
  },
  // 围护子类型
  category: {
    type: Number
  },
  showMaterial: {
    type: Boolean,
    default: true
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
