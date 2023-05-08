<template>
  <common-table :data="list" :max-height="maxHeight" empty-text="暂无数据" style="width: 100%">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column :show-overflow-tooltip="true" prop="name" label="名称" />
    <!-- <el-table-column :show-overflow-tooltip="true" prop="monomer.name" label="单体" /> -->
    <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" />
    <!-- <el-table-column :show-overflow-tooltip="true" prop="plate" label="板型" /> -->
    <!-- <el-table-column :show-overflow-tooltip="true" prop="color" label="颜色" /> -->
    <el-table-column :show-overflow-tooltip="true" prop="thickness" :label="`厚度\n(mm)`" align="center">
      <template v-slot="scope">
        {{ toFixed(scope.row.thickness, DP.MES_ENCLOSURE_T__MM) }}
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="width" :label="`有效宽度\n(mm)`" align="center">
      <template v-slot="scope">
        {{ toFixed(scope.row.width, DP.MES_ENCLOSURE_W__MM) }}
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="length" :label="`长度\n(mm)`" align="center">
      <template v-slot="scope">
        {{ toFixed(scope.row.length, DP.MES_ENCLOSURE_L__MM) }}
      </template>
    </el-table-column>
    <el-table-column prop="showQuantity" label="数量" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="totalMete" :label="`总量\n(${measureUnit})`" align="center" width="100px">
      <template v-slot="scope">
        <span>{{ scope.row.totalMete }}</span>
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
  },
  measureUnit: {
    type: String,
    default: 'm'
  }
})
</script>
