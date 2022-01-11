<template>
  <el-table-column
    v-if="showSpecification"
    :key="`${field}.specification`"
    :prop="`${field}.specification`"
    label="规格"
    width="270"
    align="center"
    :fixed="fixed"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <el-tooltip :content="specTip(getInfo(row))" placement="top">
        <span v-empty-text>{{ specFormat(getInfo(row)) }}</span>
      </el-tooltip>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed, inject } from 'vue'
import { isBlank } from '@/utils/data-type'
import { specFormat, specTip } from '@/utils/wms/spec-format'

const props = defineProps({
  specMerge: {
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  },
  columns: {
    type: Object
  },
  fixed: {
    // 定位
    type: String
  },
  field: {
    // 字段
    type: String,
    default: 'material'
  }
})

const getInfo = inject('getInfo')
const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.specification`))
</script>
