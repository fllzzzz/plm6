<template>
  <template v-if="props.specMerge">
    <el-table-column
      v-if="showSpecification"
      key="specificationMerge"
      prop="specificationMerge"
      label="规格"
      align="center"
      min-width="180px"
      :fixed="fixed"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <el-tooltip :content="specTip(row)" placement="left">
          <span>{{ specFormat(row) }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
  </template>
  <template v-else>
    <el-table-column
      v-if="showSpecification"
      key="specification_2"
      prop="specification"
      label="规格"
      align="center"
      min-width="180px"
      :fixed="fixed"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" :disabled="!row.specificationLabels" placement="left">
          {{ row.specification }}
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
