<template>
  <template v-if="props.specMerge">
    <el-table-column
      v-if="showSpecification"
      key="specification"
      prop="specification"
      label="规格"
      align="center"
      width="200px"
      :fixed="fixed"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <el-tooltip :content="specTip(row)" placement="top">
          <span v-empty-text="specFormat(row)" />
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
      width="100px"
      :fixed="fixed"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" :disabled="!row.specificationLabels" placement="top">
          <span v-empty-text="row.specification" />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column
      v-if="showThickness"
      key="thickness"
      prop="thickness"
      align="center"
      width="100px"
      :label="`厚 (${baseUnit.thickness.unit})`"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <span v-to-fixed="{ val: row.thickness, dp: baseUnit.thickness.precision }" />
      </template>
    </el-table-column>
    <el-table-column
      v-if="showWidth"
      key="width"
      prop="width"
      align="center"
      width="120px"
      :label="`宽 (${baseUnit.width.unit})`"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <span v-to-fixed="{ val: row.width, dp: baseUnit.width.precision }" />
      </template>
    </el-table-column>
    <el-table-column
      v-if="showLength"
      key="length"
      prop="length"
      align="center"
      width="120px"
      :label="`长 (${baseUnit.length.unit})`"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <span v-to-fixed="{ val: row.length, dp: baseUnit.length.precision }" />
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
  showWidth: {
    type: Boolean,
    default: true
  },
  showLength: {
    type: Boolean,
    default: true
  },
  showThickness: {
    type: Boolean,
    default: true
  }
})

// 当前分类基础单位
const { loaded, baseUnit } = useMatBaseUnit(props.basicClass)

const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible('specification'))
const showThickness = computed(() => props.showThickness && loaded.value && (isBlank(props.columns) || props.columns.visible('thickness')))
const showWidth = computed(() => props.showWidth && loaded.value && (isBlank(props.columns) || props.columns.visible('width')))
const showLength = computed(() => props.showLength && loaded.value && (isBlank(props.columns) || props.columns.visible('length')))
</script>
