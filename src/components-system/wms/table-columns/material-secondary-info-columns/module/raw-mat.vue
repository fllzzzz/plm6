<template>
  <el-table-column v-if="showBrand" prop="brand" label="品牌" align="left" min-width="100px" :fixed="fixed">
    <template #default="{ row }">
      <span v-empty-text>{{ row.brand }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
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
