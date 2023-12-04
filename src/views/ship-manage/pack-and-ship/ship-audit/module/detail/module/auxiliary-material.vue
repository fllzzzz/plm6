<template>
  <common-table :data="list" :show-empty:symbol="false" :max-height="maxHeight" empty-text="暂无数据" style="width: 100%">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <!-- <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" /> -->
    <el-table-column :show-overflow-tooltip="true" prop="monomer.name" label="单体">
      <template #default="{ row }">
        <span>{{ row.monomer ? row.monomer?.name : '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="name" label="名称">
      <template #default="{row}">
          <table-cell-tag v-if="row.boolReturn" name="退量" color="#f56c6c"/>
          <span>{{ row.name }}</span>
        </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" />
    <el-table-column :show-overflow-tooltip="true" prop="measureUnit" label="计量单位" align="center" />
    <el-table-column prop="showQuantity" label="数量" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="accountingUnit" label="核算单位" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="shipMete" label="核算量" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="unitPrice" label="单价(元)">
      <template v-slot="scope">
        {{ toFixed(scope.row.unitPrice, 4) }}
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="totalPrice" label="总价(元)">
      <template v-slot="scope">
        {{ toFixed(scope.row.totalPrice, 4) }}
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps } from 'vue'
import { toFixed } from '@/utils/data-type'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

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
