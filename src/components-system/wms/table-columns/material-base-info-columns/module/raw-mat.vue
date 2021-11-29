<template>
  <el-table-column prop="serialNumber" label="编号" align="center" width="110px" fixed="left" />
  <el-table-column prop="classifyFullName" label="物料种类" align="center" :width="basicClass > 7 ? 250 : 140" fixed="left" />
    <el-table-column prop="specification" label="规格" align="center" width="250px" fixed="left">
      <template #default="{ row }">
        <el-tooltip :content="specTip(row)" placement="top">
          <span>{{ specFormat(row) }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
</template>

<script setup>
import { defineProps } from 'vue'
import { specFormat, specTip } from '@/utils/wms/spec-format'

// eslint-disable-next-line no-unused-vars
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
