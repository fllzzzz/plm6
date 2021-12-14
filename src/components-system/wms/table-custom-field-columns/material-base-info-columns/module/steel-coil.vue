<template>
  <el-table-column v-if="showClassifyFullName" :prop="`${field}.classifyFullName`" label="物料种类" align="center" width="120px" :fixed="fixed">
    <template #default="{ row }">
      <span v-empty-text>{{ getInfo(row, 'classifyFullName') }}</span>
    </template>
  </el-table-column>
  <template v-if="props.specMerge">
    <el-table-column v-if="showSpecification" :prop="`${field}.specification`" label="规格" align="center" width="220px" :fixed="fixed">
      <template #default="{ row }">
        <el-tooltip :content="specTip(getInfo(row))" placement="top">
          <span v-empty-text>{{ specFormat(getInfo(row)) }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
  </template>
  <template v-else>
    <el-table-column v-if="showSpecification" :prop="`${field}.specification`" label="规格" align="center" width="100px" :fixed="fixed">
      <template #default="{ row }">
        <el-tooltip :content="getInfo(row, 'specificationLabels')" :disabled="!getInfo(row, 'specificationLabels')" placement="top">
          <span v-empty-text>{{ getInfo(row, 'specification') }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column v-if="showThickness" :prop="`${field}.thickness`" align="center" width="100px" :label="`厚 (mm)`">
      <template #default="{ row }">
        <span v-empty-text>{{ getInfo(row, 'thickness') }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="showWidth" :prop="`${field}.width`" align="center" width="120px" :label="`宽 (mm)`">
      <template #default="{ row }">
        <span v-empty-text>{{ getInfo(row, 'width') }}</span>
      </template>
    </el-table-column>
    <!-- <el-table-column v-if="showLength" :prop="`${field}.length`" align="center" width="120px" :label="`长 (mm)`">
      <template #default="{ row }">
        <span v-empty-text>{{ row.length }}</span>
      </template>
    </el-table-column> -->
    <el-table-column v-if="showColor" :prop="`${field}.color`" align="center" width="120px" :label="`颜色`">
      <template #default="{ row }">
        <span v-empty-text>{{ getInfo(row, 'color') }}</span>
      </template>
    </el-table-column>
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

const showClassifyFullName = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.classifyFullName`))
const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.specification`))
const showThickness = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.thickness`))
const showWidth = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.width`))
// const showLength = computed(() => isBlank(props.columns) || props.columns.visible('length'))
const showColor = computed(() => isBlank(props.columns) || props.columns.visible(`${props.field}.color`))
</script>
