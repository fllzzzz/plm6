<template>
  <common-table :data="tableData" :max-height="maxHeight" style="width: 100%">
    <el-table-column align="center" width="70">
      <template #default="{ row }">
        <span>{{ row.title }}</span>
      </template>
    </el-table-column>
    <el-table-column label="编号" prop="serialNumber" show-overflow-tooltip align="center" min-width="120" />
    <el-table-column label="规格" prop="specification" show-overflow-tooltip align="left" min-width="150" />
    <el-table-column label="长度" prop="length" show-overflow-tooltip align="center" width="100" />
    <el-table-column label="总数量" prop="quantity" show-overflow-tooltip align="center" width="100" />
    <el-table-column label="单重(kg)" prop="netWeight" show-overflow-tooltip align="center" width="100" />
    <el-table-column label="总重(kg)" prop="totalNetWeight" show-overflow-tooltip align="center" width="100" />
  </common-table>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { toPrecision } from '@/utils/data-type'

const props = defineProps({
  newArtifact: {
    type: Object,
    default: () => {}
  },
  oldArtifact: {
    type: Object,
    default: () => {}
  }
})

const tableData = computed(() => {
  const diff = {
    length: getDiffValue('length'),
    quantity: getDiffValue('quantity'),
    netWeight: getDiffValue('netWeight', 2),
    totalNetWeight: getDiffValue('totalNetWeight', 2)
  }
  return [
    { title: '原构件', ...props.oldArtifact },
    { title: '现构件', ...props.newArtifact },
    { title: '差异', ...diff }
  ]
})

function getDiffValue(field, precision = 0) {
  return toPrecision((props.oldArtifact?.[field] || 0) - (props.newArtifact?.[field] || 0), precision)
}
</script>

<style lang="scss" scoped></style>
