<template>
  <template v-if="props.specMerge">
    <el-table-column
      v-if="showSpecification"
      key="specificationMerge"
      prop="specificationMerge"
      label="规格"
      align="center"
      width="260px"
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
      width="140px"
      :fixed="fixed"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" :disabled="!row.specificationLabels" placement="left">
          {{ row.specification }}
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
      :fixed="fixed"
      :sortable="sortable"
    />
    <el-table-column
      v-if="showWidth"
      key="width"
      prop="width"
      align="center"
      width="120px"
      :label="`宽 (${baseUnit.width.unit})`"
      show-overflow-tooltip
      :fixed="fixed"
    />
    <el-table-column v-if="showColor" prop="color" align="center" width="120px" :label="`颜色`" show-overflow-tooltip :fixed="fixed" />
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
  showThickness: {
    type: Boolean,
    default: true
  },
  sortable: {
    type: [String, Boolean],
    default: false
  }
})

// 当前分类基础单位
const { loaded, baseUnit } = useMatBaseUnit(props.basicClass)

const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible('specification'))
const showThickness = computed(() => props.showThickness && loaded.value && (isBlank(props.columns) || props.columns.visible('thickness')))
const showWidth = computed(() => props.showWidth && loaded.value && (isBlank(props.columns) || props.columns.visible('width')))
const showColor = computed(() => isBlank(props.columns) || props.columns.visible('color'))
</script>
