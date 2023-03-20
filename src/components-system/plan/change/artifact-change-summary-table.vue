<template>
  <common-table
    :header-cell-style="() => `background:#fff;font-weight: bold;color: #333333;`"
    :data="artifactCompareList"
    style="width: 100%"
    :span-method="spanMethod"
  >
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column label="原始清单" align="center">
      <el-table-column label="编号" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.oldArtifact?.serialNumber || '/' }}</span>
        </template>
      </el-table-column>
      <el-table-column label="规格" show-overflow-tooltip align="left" min-width="120">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.oldArtifact?.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column label="长度(mm)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.oldArtifact?.length }}</span>
        </template>
      </el-table-column>
      <el-table-column label="数量" prop="oldQuantity" show-overflow-tooltip align="center" min-width="90">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row.oldQuantity || row?.oldArtifact?.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column label="单重(kg)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.oldArtifact?.netWeight }}</span>
        </template>
      </el-table-column>
    </el-table-column>
    <el-table-column label="构件状态" align="center" min-width="400">
      <template #default="{ row: { sourceRow: row } }">
        <production-status-box
          v-if="row.oldArtifact"
          :processList="row.oldArtifact?.processSummaryList || []"
          :processObj="row.processSummary"
        />
        <span v-else>/</span>
      </template>
    </el-table-column>
    <el-table-column label="变更后清单" align="center">
      <el-table-column label="编号" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.newArtifact?.serialNumber || '/' }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column label="规格" show-overflow-tooltip align="left" min-width="120">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.newArtifact?.specification }}</span>
        </template>
      </el-table-column> -->
      <el-table-column label="长度(mm)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.newArtifact?.length }}</span>
        </template>
      </el-table-column>
      <el-table-column label="数量" show-overflow-tooltip align="center" min-width="90">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.newArtifact?.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column label="单重(kg)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.newArtifact?.netWeight }}</span>
        </template>
      </el-table-column>
    </el-table-column>
    <el-table-column label="差异" align="center">
      <el-table-column label="长度" show-overflow-tooltip align="center" min-width="90">
        <template #default="{ row: { sourceRow: row } }">
          <cell-compare-preview :value="row.diffQuantity" />
        </template>
      </el-table-column>
      <el-table-column label="数量" show-overflow-tooltip align="center" min-width="90">
        <template #default="{ row: { sourceRow: row } }">
          <cell-compare-preview :value="row.diffQuantity" />
        </template>
      </el-table-column>
      <el-table-column label="总重量(kg)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <cell-compare-preview :value="row.diffTotalWeight" />
        </template>
      </el-table-column>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps } from 'vue'
import { isBlank } from '@/utils/data-type'
import cellComparePreview from '@comp-common/cell-compare-preview'
import productionStatusBox from './production-status-box'

defineProps({
  artifactCompareList: {
    type: Array,
    default: () => []
  }
})

function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (isBlank(row.oldArtifact)) {
    if (columnIndex === 1) {
      return {
        rowspan: 1,
        colspan: 6
      }
    } else if (columnIndex > 1 && columnIndex <= 6) {
      return {
        rowspan: 0,
        colspan: 0
      }
    }
  }
  if (isBlank(row.newArtifact)) {
    if (columnIndex === 7) {
      return {
        rowspan: 1,
        colspan: 4
      }
    } else if (columnIndex > 8 && columnIndex <= 11) {
      return {
        rowspan: 0,
        colspan: 0
      }
    }
  }
}
</script>

<style lang="scss" scoped></style>
