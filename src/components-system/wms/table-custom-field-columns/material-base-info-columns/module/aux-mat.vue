<template>
  <template v-if="props.specMerge">
    <el-table-column
      v-if="showSpecification"
      :key="`${field}.specification`"
      :prop="`${field}.specification`"
      label="规格"
      align="center"
      min-width="180px"
      :fixed="fixed"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <el-tooltip :content="specTip(getInfo(row))" placement="left">
          {{ specFormat(getInfo(row)) }}
        </el-tooltip>
      </template>
    </el-table-column>
  </template>
  <template v-else>
    <el-table-column
      v-if="showSpecification"
      :key="`${field}.specification`"
      :prop="`${field}.specification`"
      label="规格"
      align="center"
      min-width="180px"
      :fixed="fixed"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <el-tooltip :content="getInfo(row, 'specificationLabels')" :disabled="!getInfo(row, 'specificationLabels')" placement="left">
          {{ getInfo(row, 'specification') }}
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column
      v-if="showColor"
      :key="`${field}.color`"
      :prop="`${field}.color`"
      align="center"
      width="120px"
      :label="`颜色`"
      show-overflow-tooltip
    />
  </template>
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
const showColor = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.color`))
</script>
