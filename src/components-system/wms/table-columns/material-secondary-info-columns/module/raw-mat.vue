<template>
  <el-table-column
    v-if="showBrand"
    key="brand"
    prop="brand"
    label="品牌"
    align="left"
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

const showBrand = computed(() => isBlank(props.columns) || props.columns.visible('brand'))
</script>
