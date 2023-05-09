<template>
  <common-table :data="list" :max-height="maxHeight" :show-empty-symbol="false" empty-text="暂无数据" style="width: 100%">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column :show-overflow-tooltip="true" prop="monomer.name" label="单体">
      <template #default="{ row }">
        <span>{{ row.monomer ? row.monomer?.name : '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="area.name" label="区域">
      <template #default="{ row }">
        <span>{{ row.area ? row.area?.name : '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="name" label="名称" />
    <el-table-column prop="measureUnit" label="单位" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" />
    <el-table-column prop="shipQuantity" label="数量" align="center" />
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
