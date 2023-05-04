<template>
  <common-table :data="list" :max-height="maxHeight" empty-text="暂无数据" style="width: 100%">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column :show-overflow-tooltip="true" prop="name" label="名称" />
    <!-- <el-table-column :show-overflow-tooltip="true" prop="monomer.name" label="单体" /> -->
    <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" />
    <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" min-width="120" />
    <!-- <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" /> -->
    <!-- <el-table-column :show-overflow-tooltip="true" prop="material" label="材质" align="center" /> -->
    <el-table-column :show-overflow-tooltip="true" prop="length" :label="`长度\n(mm)`" align="center">
      <template v-slot="scope">
        {{ toFixed(scope.row.length, DP.MES_ARTIFACT_L__MM) }}
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="weight" :label="`单重\n（kg）`" align="center">
      <template v-slot="scope">
        {{ toFixed(scope.row.weight, DP.COM_WT__KG) }}
      </template>
    </el-table-column>
    <el-table-column prop="showQuantity" label="数量" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="totalLength" :label="`总长\n(m)`" align="center">
      <template v-slot="scope">
        {{ toFixed(scope.row.totalLength, DP.MES_ARTIFACT_L__M) }}
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="totalMete" :label="`总重\n（t）`" align="center">
      <template v-slot="scope">
        {{ toFixed(scope.row.totalMete, DP.COM_WT__T) }}
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="unitPrice" :label="`单价\n（元）`" align="center">
      <template v-slot="scope">
        {{ toFixed(scope.row.unitPrice, DP.YUAN) }}
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="totalPrice" :label="`总价\n（元）`" align="center">
      <template v-slot="scope">
        {{ toFixed(scope.row.totalPrice, DP.YUAN) }}
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@/utils/data-type'

defineProps({
  list: {
    type: Array,
    default: () => []
  },
  maxHeight: {
    type: [String, Number],
    default: undefined
  }
})
</script>
