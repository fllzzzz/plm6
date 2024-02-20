<template>
  <template v-if="props.specMerge">
    <el-table-column
      v-if="showSpecification"
      key="specificationMerge"
      prop="specificationMerge"
      label="规格"
      align="center"
      width="250px"
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
      :label="specOnly?'材质':'规格'"
      align="center"
      width="250px"
      :fixed="fixed"
      show-overflow-tooltip
    >
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" :disabled="!row.specificationLabels" placement="left">
          {{ row.specification }}
        </el-tooltip>
      </template>
    </el-table-column>
    <template v-if="specOnly">
      <el-table-column
        v-if="showOtherMerge"
        key="otherMerge"
        prop="otherMerge"
        label="规格"
        align="center"
        width="200px"
        :fixed="fixed"
        show-overflow-tooltip
      >
        <template #default="{ row }">
          <el-tooltip :content="otherTip(row)" placement="left">
            <span>{{ otherFormat(row) }}</span>
          </el-tooltip>
        </template>
      </el-table-column>
    </template>
    <template v-else>
      <el-table-column
        v-if="showLength"
        key="length"
        prop="length"
        align="center"
        width="120px"
        :label="`长 (${baseUnit.length.unit})`"
        :fixed="fixed"
        show-overflow-tooltip
      />
      </template>

  </template>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'
import { specFormat, specTip } from '@/utils/wms/spec-format'
import { otherFormat, otherTip } from '@/utils/wms/other-format'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'

const props = defineProps({
  specMerge: {
    type: Boolean,
    default: false
  },
  specOnly: {
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
  },
  sortable: {
    type: [String, Boolean],
    default: false
  }
})

// 当前分类基础单位
const { loaded, baseUnit } = useMatBaseUnit(props.basicClass)

const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible('specification'))
const showOtherMerge = computed(() => isBlank(props.columns) || props.columns.visible('otherMerge'))
const showLength = computed(() => props.showLength && loaded.value && (isBlank(props.columns) || props.columns.visible('length')))
</script>
