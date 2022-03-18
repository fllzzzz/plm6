<template>
  <el-form ref="form" :model="data" label-width="40px" style="display: flex" label-position="left">
    <el-form-item label="项目" style="margin-right: 20px">
      <span v-parse-project="{ project: data.project, onlyShortName: true }" v-empty-text />
    </el-form-item>
    <el-form-item label="单体" style="margin-right: 20px">
      <span v-empty-text>{{ data.monomer?.name }}</span>
    </el-form-item>
    <el-form-item label="区域">
      <span v-empty-text>{{ data.areaDetail?.name }}</span>
    </el-form-item>
  </el-form>
  <common-table :data="[data]">
    <productType-full-info-columns
      :productType="productType"
      :unitNewLine="false"
      :unShowField="['totalNetWeight', 'totalGrossWeight', 'remark']"
    />
    <el-table-column prop="oldQuantity" :show-overflow-tooltip="true" label="原清单数量" width="100" align="center">
      <template #default="{ row }">
        <span v-empty-text="row.oldQuantity" />
      </template>
    </el-table-column>
    <el-table-column prop="newQuantity" :show-overflow-tooltip="true" label="变更后清单数量" width="110" align="center">
      <template #default="{ row }">
        <span v-empty-text="row.newQuantity" />
      </template>
    </el-table-column>
    <el-table-column v-if="showTaskQ" prop="taskQuantity" :show-overflow-tooltip="true" label="任务数量" width="100" align="center">
      <template #default="{ row }">
        <span v-empty-text="row.taskQuantity" />
      </template>
    </el-table-column>
    <el-table-column
      v-if="showSchedulingQ"
      prop="totalSchedulingQuantity"
      :show-overflow-tooltip="true"
      label="排产数量"
      width="100"
      align="center"
    >
      <template #default="{ row }">
        <span v-empty-text="row.totalSchedulingQuantity" />
      </template>
    </el-table-column>
    <el-table-column
      v-if="showInProductionQ"
      prop="totalInProductionQuantity"
      :show-overflow-tooltip="true"
      label="已生产数量"
      width="100"
      align="center"
    >
      <template #default="{ row }">
        <span v-empty-text="row.totalInProductionQuantity" />
      </template>
    </el-table-column>
    <el-table-column v-if="showHandleQ" prop="handleQuantity" :show-overflow-tooltip="true" label="需处理数量" width="100" align="center">
      <template #default="{ row }">
        <span v-empty-text="row.handleQuantity"></span>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps } from 'vue'

import productTypeFullInfoColumns from '@comp-mes/table-columns/productType-full-info-columns'

defineProps({
  data: {
    type: Object
  },
  productType: {
    type: Number
  },
  showTaskQ: {
    type: Boolean,
    default: false
  },
  showSchedulingQ: {
    type: Boolean,
    default: false
  },
  showInProductionQ: {
    type: Boolean,
    default: false
  },
  showHandleQ: {
    type: Boolean,
    default: false
  }
})
</script>
