<template>
  <el-table-column prop="serialNumber" label="编号" align="center" width="110px" fixed="left" />
  <el-table-column prop="classifyFullName" label="物料种类" align="center" width="120px" fixed="left" />
  <template v-if="props.specMerge">
    <el-table-column prop="specification" label="规格" align="center" width="200px" fixed="left">
      <template #default="{ row }">
        <el-tooltip :content="`${row.specificationLabels ? row.specificationLabels + '|' : '' } 厚(mm) * 宽(mm) * 长(mm)`" placement="top">
          <span v-suffix="' | '">{{ row.specification }}</span>
          <span>{{ `${row.thickness} * ${row.width} * ${row.length}` }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
  </template>
  <template v-else>
    <el-table-column prop="specification" label="规格" align="center" width="100px" fixed="left">
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" :disabled="!row.specificationLabels" placement="top">
          <span>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="thickness" align="center" width="100px" :label="`厚 (mm)`">
      <template #default="{ row }">
        <span>{{ row.thickness }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="width" align="center" width="120px" :label="`宽 (mm)`">
      <template #default="{ row }">
        <span>{{ row.width }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="length" align="center" width="120px" :label="`长 (mm)`">
      <template #default="{ row }">
        <span>{{ row.length }}</span>
      </template>
    </el-table-column>
  </template>
</template>

<script setup>
import { defineProps } from 'vue'

const props = defineProps({
  specMerge: {
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  }
})
</script>
