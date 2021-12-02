<template>
  <el-table-column prop="serialNumber" label="编号" align="center" width="110px" fixed="left" >
    <template #default="{ row }">
      <factory-table-cell-tag v-if="props.showFactory" :id="row.factory ? row.factory.id : row.factoryId" />
      <span>{{ row.serialNumber }}</span>
    </template>
  </el-table-column>
  <el-table-column prop="classifyFullName" label="物料种类" align="center" min-width="180px" fixed="left" />
  <template v-if="props.specMerge">
    <el-table-column prop="specification" label="规格" align="center" min-width="180px" fixed="left">
      <template #default="{ row }">
        <el-tooltip :content="specTip(row)" placement="top">
          <span>{{ specFormat(row) }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
  </template>
  <template v-else>
    <el-table-column prop="specification" label="规格" align="center" min-width="180px" fixed="left">
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" :disabled="!row.specificationLabels" placement="top">
          <span>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="color" align="center" width="120px" :label="`颜色`" />
  </template>
</template>

<script setup>
import { defineProps } from 'vue'
import { specFormat, specTip } from '@/utils/wms/spec-format'

const props = defineProps({
  specMerge: {
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  },
  showFactory: {
    type: Boolean,
    default: false
  }
})
</script>
