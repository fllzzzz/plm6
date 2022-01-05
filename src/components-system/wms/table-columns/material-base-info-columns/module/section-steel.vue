<template>
  <template v-if="props.specMerge">
    <el-table-column
      v-if="showSpecification"
      prop="specification"
      label="规格"
      align="center"
      width="250px"
      :fixed="fixed"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <el-tooltip :content="specTip(row)" placement="top">
          <span v-empty-text>{{ specFormat(row) }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
  </template>
  <template v-else>
    <el-table-column
      v-if="showSpecification"
      prop="specification"
      label="规格"
      align="center"
      width="250px"
      :fixed="fixed"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" :disabled="!row.specificationLabels" placement="top">
          <span v-empty-text>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column
      v-if="showLength"
      prop="length"
      align="center"
      width="120px"
      :label="`长 (${baseUnit.length.unit})`"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <span v-to-fixed="baseUnit.length.precision">{{ row.length }}</span>
      </template>
    </el-table-column>
  </template>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'
import { specFormat, specTip } from '@/utils/wms/spec-format'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'

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
  showLength: {
    type: Boolean,
    default: true
  }
})

// 当前分类基础单位
const { loaded, baseUnit } = useMatBaseUnit(props.basicClass)

const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible('specification'))
const showLength = computed(() => props.showLength && loaded.value && (isBlank(props.columns) || props.columns.visible('length')))
</script>
