<template>
  <template v-if="props.specMerge">
    <el-table-column
      v-if="showSpecification"
      :key="`${field}.specification`"
      :prop="`${field}.specification`"
      label="规格"
      align="center"
      width="200px"
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
  <template v-else>
    <el-table-column
      v-if="showSpecification"
      :key="`${field}.specification`"
      :prop="`${field}.specification`"
      label="规格"
      align="center"
      width="100px"
      :fixed="fixed"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <el-tooltip :content="getInfo(row, 'specificationLabels')" :disabled="!getInfo(row, 'specificationLabels')" placement="top">
          <span v-empty-text>{{ getInfo(row, 'specification') }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column
      v-if="showThickness"
      :key="`${field}.thickness`"
      :prop="`${field}.thickness`"
      align="center"
      width="100px"
      :label="`厚 (${baseUnit.thickness.unit})`"
      show-overflow-tooltip
      :fixed="fixed"
    >
      <template #default="{ row }">
        <span v-to-fixed="baseUnit.thickness.precision">{{ getInfo(row, 'thickness') }}</span>
      </template>
    </el-table-column>
    <el-table-column
      v-if="showWidth"
      :key="`${field}.width`"
      :prop="`${field}.width`"
      align="center"
      width="120px"
      :label="`宽 (${baseUnit.width.unit})`"
      show-overflow-tooltip
      :fixed="fixed"
    >
      <template #default="{ row }">
        <span v-to-fixed="baseUnit.width.precision">{{ getInfo(row, 'width') }}</span>
      </template>
    </el-table-column>
    <el-table-column
      v-if="showLength"
      :key="`${field}.length`"
      :prop="`${field}.length`"
      align="center"
      width="120px"
      :label="`长 (${baseUnit.length.unit})`"
      show-overflow-tooltip
      :fixed="fixed"
    >
      <template #default="{ row }">
        <span v-to-fixed="baseUnit.length.precision">{{ getInfo(row, 'length') }}</span>
      </template>
    </el-table-column>
  </template>
</template>

<script setup>
import { defineProps, computed, inject } from 'vue'
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
  field: {
    // 字段
    type: String,
    default: 'material'
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

const getInfo = inject('getInfo')

const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.specification`))
const showThickness = computed(
  () => props.showThickness && loaded.value && (isBlank(props.columns) || props.columns.visible(`${props.field}.thickness`))
)
const showWidth = computed(
  () => props.showWidth && loaded.value && (isBlank(props.columns) || props.columns.visible(`${props.field}.width`))
)
const showLength = computed(
  () => props.showLength && loaded.value && (isBlank(props.columns) || props.columns.visible(`${props.field}.length`))
)
</script>
