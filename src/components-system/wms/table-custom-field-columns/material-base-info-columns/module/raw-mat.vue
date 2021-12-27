<template>
  <!-- 钢材宽度100， 其他180 :min-width="props.basicClass > STEEL_ENUM ? 180 : undefined" -->
  <el-table-column
    v-if="showClassifyFullName"
    :prop="`${field}.classifyFullName`"
    label="物料种类"
    align="center"
    show-overflow-tooltip
    :width="classifyFullNameWidth"
    :fixed="fixed"
  >
    <template #default="{ row }">
      <span v-empty-text>{{ getInfo(row, 'classifyFullName') }}</span>
    </template>
  </el-table-column>
  <el-table-column
    v-if="showSpecification"
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
import { STEEL_ENUM } from '@/settings/config'

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

// 物料全名宽度
const classifyFullNameWidth = computed(() => {
  // 基础分类不存在，或基础分类不为钢材，则宽度为100
  return !props.basicClass || props.basicClass > STEEL_ENUM ? 250 : 100
})
const showClassifyFullName = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.classifyFullName`))
const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.specification`))
</script>
