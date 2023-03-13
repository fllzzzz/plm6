<template>
  <common-table
    :header-cell-style="() => `font-weight: bold;color: #333333;`"
    :data="tableData"
    style="width: 100%"
  >
    <el-table-column align="center" width="70">
      <template #default="{ row }">
        <span style="font-weight: bold">{{ row.title }}</span>
      </template>
    </el-table-column>
    <el-table-column label="编号" prop="serialNumber" show-overflow-tooltip align="center" min-width="120" />
    <el-table-column label="规格" prop="specification" show-overflow-tooltip align="left" min-width="150" />
    <el-table-column label="长度(mm)" prop="length" show-overflow-tooltip align="center" width="100">
      <template #default="{ row }">
        <cell-compare-preview :value="row.length" v-if="row.isDiff" />
        <span v-else>{{ row.length }}</span>
      </template>
    </el-table-column>
    <el-table-column label="总数量" prop="quantity" show-overflow-tooltip align="center" width="100">
      <template #default="{ row }">
        <cell-compare-preview :value="row.quantity" v-if="row.isDiff" />
        <span v-else>{{ row.quantity }}</span>
      </template>
    </el-table-column>
    <el-table-column label="单重(kg)" prop="netWeight" show-overflow-tooltip align="center" width="100">
      <template #default="{ row }">
        <cell-compare-preview :value="row.netWeight" v-if="row.isDiff" />
        <span v-else>{{ row.netWeight }}</span>
      </template>
    </el-table-column>
    <el-table-column label="总重(kg)" prop="totalNetWeight" show-overflow-tooltip align="center" width="100">
      <template #default="{ row }">
        <cell-compare-preview :value="row.totalNetWeight" v-if="row.isDiff" />
        <span v-else>{{ row.totalNetWeight }}</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { toPrecision } from '@/utils/data-type'
import cellComparePreview from '@comp-common/cell-compare-preview'

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
    { title: '原构件', isDiff: false, ...props.oldArtifact },
    { title: '现构件', isDiff: false, ...props.newArtifact },
    { title: '差异', isDiff: true, ...diff }
  ]
})

function getDiffValue(field, precision = 0) {
  return toPrecision((props.oldArtifact?.[field] || 0) - (props.newArtifact?.[field] || 0), precision)
}
</script>

<style lang="scss" scoped></style>
