<template>
  <el-table-column prop="serialNumber" label="编号" align="center" width="110px" fixed="left">
    <template #default="{ row }">
      <factory-table-cell-tag v-if="props.showFactory" :id="row.factory ? row.factory.id : row.factoryId" />
      <span v-empty-text>{{ row.serialNumber }}</span>
    </template>
  </el-table-column>
  <!-- 钢材宽度100， 其他180 :min-width="props.basicClass > STEEL_ENUM ? 180 : undefined"-->
  <el-table-column
    prop="classifyFullName"
    label="物料种类"
    align="center"
    :width="props.basicClass > STEEL_ENUM ? 250 : 100"
    fixed="left"
  />
  <el-table-column prop="specification" label="规格" align="center" show-overflow-tooltip width="270" fixed="left">
    <template #default="{ row }">
      <el-tooltip :content="specTip(row)" placement="top">
        <span v-empty-text>{{ specFormat(row) }}</span>
      </el-tooltip>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps } from 'vue'
import { specFormat, specTip } from '@/utils/wms/spec-format'
import { STEEL_ENUM } from '@/settings/config'
import factoryTableCellTag from '@comp-base/factory-table-cell-tag.vue'

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
