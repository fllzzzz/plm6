<template>
  <template v-if="props.specMerge">
    <el-table-column v-if="showSpecification" prop="specification" label="规格" align="center" min-width="180px" :fixed="fixed">
      <template #default="{ row }">
        <el-tooltip :content="specTip(row)" placement="top">
          <span v-empty-text>{{ specFormat(row) }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
  </template>
  <template v-else>
    <el-table-column v-if="showSpecification" prop="specification" label="规格" align="center" min-width="180px" :fixed="fixed">
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" :disabled="!row.specificationLabels" placement="top">
          <span v-empty-text>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
  </template>
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
