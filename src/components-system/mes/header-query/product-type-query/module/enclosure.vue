<template>
  <el-input
    v-model.trim="queryVO.name"
    size="small"
    placeholder="输入名称搜索"
    style="width: 170px"
    class="filter-item"
    clearable
    @keyup.enter="toQuery"
  />
  <el-input
    v-if="!(unShowSNVal & category)"
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
import { defineProps, defineEmits, ref, watchEffect, computed } from 'vue'
import { mesEnclosureTypeEnum } from '@enum-ms/mes'

const emit = defineEmits(['to-query'])

const props = defineProps({
  productType: {
    type: Number
  },
  // 围护子类型
  category: {
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

const unShowSNVal = computed(() => {
  return mesEnclosureTypeEnum.SANDWICH_BOARD.V
})
</script>
