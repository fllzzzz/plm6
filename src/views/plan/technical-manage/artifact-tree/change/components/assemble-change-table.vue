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
      <el-table-column label="编号" prop="oldSN" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row.oldSN || '/' }}</span>
        </template>
      </el-table-column>
      <el-table-column label="规格" show-overflow-tooltip align="left" min-width="120">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ oldObj[row.oldSN]?.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column label="长度(mm)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ oldObj[row.oldSN]?.length }}</span>
        </template>
      </el-table-column>
      <el-table-column label="数量" prop="oldQuantity" show-overflow-tooltip align="center" min-width="90">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row.oldQuantity || oldObj[row.oldSN]?.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column label="单重(kg)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ oldObj[row.oldSN]?.netWeight }}</span>
        </template>
      </el-table-column>
    </el-table-column>
    <el-table-column width="10px" />
    <el-table-column label="变更清单（仅显示涉及变更的部件）" align="center">
      <el-table-column label="编号" prop="newSN" show-overflow-tooltip align="center" min-width="100" >
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ row.newSN || '/' }}</span>
        </template>
      </el-table-column>
      <el-table-column label="规格" show-overflow-tooltip align="left" min-width="120">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ newObj[row.newSN]?.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column label="长度(mm)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ newObj[row.newSN]?.length }}</span>
        </template>
      </el-table-column>
      <el-table-column label="数量" show-overflow-tooltip align="center" min-width="90">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ newObj[row.newSN]?.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column label="单重(kg)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row: { sourceRow: row } }">
          <span>{{ newObj[row.newSN]?.netWeight }}</span>
        </template>
      </el-table-column>
    </el-table-column>
    <el-table-column width="10px" />
    <el-table-column label="差异" align="center">
      <el-table-column label="数量" show-overflow-tooltip align="center" min-width="90">
        <template #default="{ row }">
          <cell-compare-preview :value="row.diffQuantity" />
        </template>
      </el-table-column>
      <el-table-column label="总重量(kg)" show-overflow-tooltip align="center" min-width="100">
        <template #default="{ row }">
          <cell-compare-preview :value="row.diffTotalWeight" />
        </template>
      </el-table-column>
    </el-table-column>
    <el-table-column label="处理" prop="handleType" show-overflow-tooltip align="center" min-width="100" />
  </common-table>
</template>

<script setup>
import { defineProps, ref, computed } from 'vue'
import { assembleHandleMethodEnum } from './common'
import changeTypeTableColumn from './change-type-table-column'
import cellComparePreview from '@comp-common/cell-compare-preview'

const props = defineProps({
  assembleCompareList: {
    type: Array,
    default: () => []
  },
  assembleInfo: {
    type: Object,
    default: () => {}
  }
})

const oldObj = computed(() => props.assembleInfo?.needHandleOldObj || {})
const newObj = computed(() => props.assembleInfo?.needHandleNewObj || {})

const columnsDataFormat = ref([['handleType', ['parse-enum', assembleHandleMethodEnum, { bit: true, split: ' 、 ' }]]])

function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (!row.oldSN) {
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
  if (!row.newSN) {
    if (columnIndex === 8) {
      return {
        rowspan: 1,
        colspan: 5
      }
    } else if (columnIndex > 8 && columnIndex <= 12) {
      return {
        rowspan: 0,
        colspan: 0
      }
    }
  }
}
</script>

<style lang="scss" scoped></style>
