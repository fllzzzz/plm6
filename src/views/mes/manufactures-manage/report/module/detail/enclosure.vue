<template>
  <common-table :data="tableData" :max-height="maxHeight" row-key="rowId" style="width: 100%">
    <el-table-column label="序号" type="index" align="center" width="60" />
     <el-table-column v-if="isSummary" prop="project" label="项目" min-width="120px" show-overflow-tooltip>
      <template #default="{ row }">
        <span v-parse-project="{ project: row.project, onlyShortName: true }" />
      </template>
    </el-table-column>
    <el-table-column key="monomer.name" prop="monomer.name" show-overflow-tooltip label="单体">
      <template v-slot="scope">
        <span>{{ scope.row.monomer?.name }}</span>
      </template>
    </el-table-column>
    <el-table-column key="name" prop="name" show-overflow-tooltip label="名称">
      <template v-slot="scope">
        <span>{{ scope.row.name }}</span>
      </template>
    </el-table-column>
    <el-table-column key="serialNumber" prop="serialNumber" show-overflow-tooltip label="编号">
      <template v-slot="scope">
        <span>{{ scope.row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column key="plate" prop="plate" show-overflow-tooltip label="板型">
      <template v-slot="scope">
        <span>{{ scope.row.plate }}</span>
      </template>
    </el-table-column>
    <el-table-column key="color" prop="color" show-overflow-tooltip label="颜色">
      <template v-slot="scope">
        <span>{{ scope.row.color }}</span>
      </template>
    </el-table-column>
    <el-table-column key="quantity" prop="quantity" show-overflow-tooltip label="数量" align="center">
      <template v-slot="scope">
        <span>{{ scope.row.quantity }}</span>
      </template>
    </el-table-column>
    <el-table-column key="thickness" prop="thickness" show-overflow-tooltip label="厚度(mm)" align="center">
      <template v-slot="scope">
        <span>{{ toFixed(scope.row.thickness, DP.MES_ENCLOSURE_T__MM) }}</span>
      </template>
    </el-table-column>
    <el-table-column key="width" prop="width" show-overflow-tooltip label="宽度(mm)" align="center">
      <template v-slot="scope">
        <span>{{ toFixed(scope.row.width, DP.MES_ENCLOSURE_W__MM) }}</span>
      </template>
    </el-table-column>
    <el-table-column key="length" prop="length" show-overflow-tooltip label="长度(mm)" align="center">
      <template v-slot="scope">
        <span>{{ toFixed(scope.row.length, DP.MES_ARTIFACT_L__MM) }}</span>
      </template>
    </el-table-column>
    <el-table-column key="totalLength" prop="totalLength" show-overflow-tooltip label="总长度(m)" align="center">
      <template v-slot="scope">
        <span>{{ convertUnits(scope.row.totalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) }}</span>
      </template>
    </el-table-column>
    <el-table-column key="totalArea" prop="totalArea" show-overflow-tooltip label="总面积(㎡)" align="center">
      <template v-slot="scope">
        <span>{{ convertUnits(scope.row.totalArea, 'mm2','m2', DP.COM_AREA__M2) }}</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'
import { convertUnits } from '@/utils/convert/unit'

defineProps({
  tableData: {
    type: Array,
    default: () => []
  },
  maxHeight: {
    type: Number,
    default: undefined
  },
  isSummary: {
    type: Boolean,
    default: false
  }
})
</script>
