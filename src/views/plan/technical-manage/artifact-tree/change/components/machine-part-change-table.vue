<template>
  <common-table
    :header-cell-style="() => `background:#fff;font-weight: bold;color: #333333;`"
    :data="partCompareList"
    style="max-width: 1200px"
  >
    <el-table-column label="序号" type="index" align="center" width="60" />
    <change-type-table-column />
    <el-table-column label="编号" prop="serialNumber" show-overflow-tooltip align="center" min-width="120" />
    <el-table-column label="规格" prop="specification" show-overflow-tooltip align="left" min-width="150" />
    <el-table-column label="长度(mm)" prop="length" show-overflow-tooltip align="center" min-width="100" />
    <el-table-column label="单重(kg)" prop="netWeight" show-overflow-tooltip align="center" min-width="100" />
    <el-table-column label="原清单数量" prop="oldQuantity" show-overflow-tooltip align="center" min-width="120" />
    <el-table-column width="10px" v-if="!showProcess" />
    <el-table-column v-else label="零件状态" align="center" min-width="400">
      <template #default="{ row: { sourceRow: row } }">
        <production-status-box
          v-if="row.oldQuantity"
          :processList="row.processSummaryList || []"
          :processObj="row.processSummary"
        />
        <span v-else>/</span>
      </template>
    </el-table-column>
    <el-table-column label="变更后清单数量" prop="newQuantity" show-overflow-tooltip align="center" min-width="120" />
    <el-table-column width="10px" v-if="!showProcess"/>
    <el-table-column label="差异" align="center">
      <el-table-column label="数量" show-overflow-tooltip align="center" min-width="120">
        <template #default="{ row }">
          <cell-compare-preview :value="row.diffQuantity" />
        </template>
      </el-table-column>
      <el-table-column label="总重量(kg)" show-overflow-tooltip align="center" min-width="120">
        <template #default="{ row }">
          <cell-compare-preview :value="row.diffTotalWeight" />
        </template>
      </el-table-column>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps } from 'vue'
import changeTypeTableColumn from './change-type-table-column'
import cellComparePreview from '@comp-common/cell-compare-preview'
import productionStatusBox from './production-status-box'

defineProps({
  partCompareList: {
    type: Array,
    default: () => []
  },
  showProcess: {
    type: Boolean,
    default: false
  }
})
</script>

<style lang="scss" scoped></style>
