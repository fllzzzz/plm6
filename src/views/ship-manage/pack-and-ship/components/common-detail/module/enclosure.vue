<template>
  <common-table :data="list" :max-height="maxHeight" empty-text="暂无数据" style="width: 100%">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column :show-overflow-tooltip="true" prop="area.name" label="批次" />
    <!-- <el-table-column v-if="isSuspend" :show-overflow-tooltip="true" prop="area.name" label="区域" /> -->
    <el-table-column :show-overflow-tooltip="true" prop="name" label="名称" />
    <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" />
    <el-table-column :show-overflow-tooltip="true" prop="plate" label="板型" />
    <el-table-column prop="showQuantity" label="数量" align="center" />
    <!-- <el-table-column :show-overflow-tooltip="true" prop="color" label="颜色" /> -->
    <!-- <el-table-column :show-overflow-tooltip="true" prop="thickness" :label="`厚度\n(mm)`" align="center">
      <template v-slot="scope">
        {{ toFixed(scope.row.thickness, DP.MES_ENCLOSURE_T__MM) }}
      </template>
    </el-table-column> -->
    <el-table-column :show-overflow-tooltip="true" prop="surfaceArea" :label="`单面积\n(m²)`" align="center">
      <template v-slot="scope">
         {{ convertUnits(scope.row.surfaceArea, 'mm²', 'm²', DP.COM_AREA__M2) }}
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="length" :label="`单长\n(mm)`" align="center">
      <template v-slot="scope">
        {{ toFixed(scope.row.length, DP.MES_ENCLOSURE_L__MM) }}
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="totalLength" :label="`总长度\n(m)`" align="center" width="100px">
      <template v-slot="scope">
        {{ toFixed(scope.row.totalLength, DP.MES_ENCLOSURE_L__M) }}
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps } from 'vue'
import { convertUnits } from '@/utils/convert/unit'
import { DP } from '@/settings/config'
import { toFixed } from '@/utils/data-type'

defineProps({
  list: {
    type: Array,
    default: () => []
  },
  isSuspend: {
    type: Boolean,
    default: false
  },
  maxHeight: {
    type: [String, Number],
    default: undefined
  }
})
</script>
