<template>
  <common-table :data="tableData" :max-height="maxHeight" row-key="rowId" style="width: 100%">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column v-if="isSummary" prop="project" label="项目" min-width="120px" show-overflow-tooltip>
      <template #default="{ row }">
        <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
      </template>
    </el-table-column>
    <el-table-column prop="monomer.name" show-overflow-tooltip label="单体">
      <template v-slot="scope">
        <span v-empty-text>{{ scope.row.monomer?.name }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="name" show-overflow-tooltip label="名称">
      <template v-slot="scope">
        <span v-empty-text>{{ scope.row.name }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="serialNumber" show-overflow-tooltip label="编号">
      <template v-slot="scope">
        <span v-empty-text>{{ scope.row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="specification" show-overflow-tooltip label="规格">
      <template v-slot="scope">
        <span v-empty-text>{{ scope.row.specification }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="length" show-overflow-tooltip label="长度(mm)">
      <template v-slot="scope">
        <span v-empty-text>{{ toFixed(scope.row.length, DP.MES_ARTIFACT_L__MM) }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="material" show-overflow-tooltip label="材质">
      <template v-slot="scope">
        <span v-empty-text>{{ scope.row.material }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="quantity" show-overflow-tooltip label="数量" align="center">
      <template v-slot="scope">
        <span v-empty-text>{{ scope.row.quantity }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="netWeight" show-overflow-tooltip label="单净重(kg)" align="center">
      <template v-slot="scope">
        <span v-empty-text>{{ toFixed(scope.row.netWeight, DP.COM_WT__KG) }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="grossWeight" show-overflow-tooltip label="单毛重(kg)" align="center">
      <template v-slot="scope">
        <span v-empty-text>{{ toFixed(scope.row.grossWeight, DP.COM_WT__KG) }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="totalNetWeight" show-overflow-tooltip label="总净重(kg)" align="center">
      <template v-slot="scope">
        <span v-empty-text>{{ toFixed(scope.row.totalNetWeight, DP.COM_WT__KG) }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="totalGrossWeight" show-overflow-tooltip label="总毛重(kg)" align="center">
      <template v-slot="scope">
        <span v-empty-text>{{ toFixed(scope.row.totalGrossWeight, DP.COM_WT__KG) }}</span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'

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
