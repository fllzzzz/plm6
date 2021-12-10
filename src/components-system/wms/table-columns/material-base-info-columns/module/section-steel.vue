<template>
  <el-table-column v-if="showClassifyFullName" prop="classifyFullName" label="物料种类" align="center" width="120px" fixed="left" />
  <template v-if="props.specMerge">
    <el-table-column v-if="showSpecification" prop="specification" label="规格" align="center" width="200px" fixed="left">
      <template #default="{ row }">
        <el-tooltip :content="specTip(row)" placement="top">
          <span v-empty-text>{{ specFormat(row) }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
  </template>
  <template v-else>
    <el-table-column v-if="showSpecification" prop="specification" label="规格" align="center" width="200px" fixed="left">
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" :disabled="!row.specificationLabels" placement="top">
          <span v-empty-text>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column v-if="showColumn"></el-table-column>
    <el-table-column v-if="showLength" prop="length" align="center" width="120px" :label="`长 (mm)`">
      <template #default="{ row }">
        <span v-empty-text>{{ row.length }}</span>
      </template>
    </el-table-column>
  </template>
</template>

<script setup>
import { defineProps, computed, ref, nextTick } from 'vue'
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
  }
})

// TODO:等待vue或element修复bug，此处不加入空列处理会导致dom结构错误，原因未知
const showColumn = ref(true)
nextTick(() => {
  showColumn.value = false
})

const showClassifyFullName = computed(() => isBlank(props.columns) || props.columns.visible('classifyFullName'))
const showSpecification = computed(() => isBlank(props.columns) || props.columns.visible('specification'))
const showLength = computed(() => isBlank(props.columns) || props.columns.visible('length'))
</script>
