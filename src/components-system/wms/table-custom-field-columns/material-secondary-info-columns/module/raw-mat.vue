<template>
  <el-table-column v-if="showBrand" :prop="`${field}.brand`" label="品牌" align="left" min-width="100px">
    <template #default="{ row }">
      <span v-empty-text>{{ getInfo(row, 'brand') }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed, inject } from 'vue'
import { isBlank } from '@/utils/data-type'

const props = defineProps({
  columns: {
    type: Object
  },
  field: {
    // 字段
    type: String,
    default: 'material'
  }
})

const getInfo = inject('getInfo')

const showBrand = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.brand`))
</script>
