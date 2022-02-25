<template>
  <el-table-column
    v-if="showSpecification"
    key="specification"
    prop="specification"
    label="规格"
    width="200"
    align="center"
    :fixed="fixed"
    show-overflow-tooltip
  >
    <template #default="{ row }">
      <el-tooltip :content="specTip(row)" placement="left">
        <span v-empty-text="specFormat(row)" />
      </el-tooltip>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
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
  }
})

const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible('specification'))
</script>
