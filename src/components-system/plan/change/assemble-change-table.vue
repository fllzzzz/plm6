<template>
  <common-table
    :header-cell-style="() => `background:#fff;font-weight: bold;color: #333333;`"
    :data="assembleCompareList"
    style="width: 100%"
    :span-method="spanMethod"
    :data-format="columnsDataFormat"
  >
    <el-table-column label="序号" type="index" align="center" width="60" />
    <change-type-table-column />
    <el-table-column label="原清单（仅显示涉及变更的部件）" align="center">
      <el-table-column label="编号" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.oldAssemble?.serialNumber || '/' }}</span>
        </template>
      </el-table-column>
      <el-table-column label="规格" show-overflow-tooltip align="left" min-width="120">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.oldAssemble?.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column label="长度(mm)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.oldAssemble?.length }}</span>
        </template>
      </el-table-column>
      <el-table-column label="数量" prop="oldQuantity" show-overflow-tooltip align="center" min-width="90">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row.oldQuantity || row?.oldAssemble?.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column label="单重(kg)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.oldAssemble?.netWeight }}</span>
        </template>
      </el-table-column>
    </el-table-column>
    <el-table-column width="10px" v-if="!showProcess" />
    <el-table-column v-else label="部件状态" align="center" min-width="400">
      <template #default="{ row: { sourceRow: row } }">
        <production-status-box
          v-if="row.oldAssemble"
          :processList="row.oldAssemble?.processSummaryList || []"
          :processObj="row.processSummary"
        />
        <span v-else>/</span>
      </template>
    </el-table-column>
    <el-table-column label="变更清单（仅显示涉及变更的部件）" align="center">
      <el-table-column label="编号" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.newAssemble?.serialNumber || '/' }}</span>
        </template>
      </el-table-column>
      <el-table-column label="规格" show-overflow-tooltip align="left" min-width="120" v-if="!showProcess">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.newAssemble?.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column label="长度(mm)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.newAssemble?.length }}</span>
        </template>
      </el-table-column>
      <el-table-column label="数量" show-overflow-tooltip align="center" min-width="90">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.newAssemble?.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column label="单重(kg)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row?.newAssemble?.netWeight }}</span>
        </template>
      </el-table-column>
    </el-table-column>
    <el-table-column width="10px" v-if="!showProcess" />
    <el-table-column label="差异" align="center">
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
    <el-table-column label="处理" prop="handleType" show-overflow-tooltip align="center" min-width="100" />
  </common-table>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { assembleHandleMethodEnum } from './common'
import { isBlank } from '@/utils/data-type'
import changeTypeTableColumn from './change-type-table-column'
import cellComparePreview from '@comp-common/cell-compare-preview'
import productionStatusBox from './production-status-box'

const props = defineProps({
  assembleCompareList: {
    type: Array,
    default: () => []
  },
  showProcess: {
    type: Boolean,
    default: false
  }
})

const columnsDataFormat = ref([['handleType', ['parse-enum', assembleHandleMethodEnum, { bit: true, split: ' 、 ' }]]])

function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (isBlank(row.oldAssemble)) {
    if (columnIndex === 2) {
      return {
        rowspan: 1,
        colspan: 5
      }
    } else if (columnIndex > 2 && columnIndex <= 6) {
      return {
        rowspan: 0,
        colspan: 0
      }
    }
  }
  if (isBlank(row.newAssemble)) {
    if (columnIndex === 8) {
      return {
        rowspan: 1,
        colspan: !props.showProcess ? 5 : 4
      }
    } else if (columnIndex > 8 && columnIndex <= 12 && !props.showProcess) {
      return {
        rowspan: 0,
        colspan: 0
      }
    } else if (columnIndex > 8 && columnIndex <= 11 && props.showProcess) {
      return {
        rowspan: 0,
        colspan: 0
      }
    }
  }
}
</script>

<style lang="scss" scoped></style>
