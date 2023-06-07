<template>
  <el-table-column
    v-if="showLength"
    key="length"
    prop="length"
    label="长度 (mm)"
    align="center"
    min-width="100px"
    :fixed="fixed"
    show-overflow-tooltip
  />
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
  basicClass: {
    type: Number
  },
  columns: {
    type: Object
  },
  fixed: {
    // 定位
    type: String
  }
})

const showLength = computed(() => isBlank(props.columns) || props.columns.visible('length'))
</script>
