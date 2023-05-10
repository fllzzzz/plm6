<template>
  <common-table :data="list" :max-height="maxHeight" empty-text="暂无数据" style="width: 100%">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column :show-overflow-tooltip="true" prop="name" label="名称" />
    <!-- <el-table-column :show-overflow-tooltip="true" prop="monomer.name" label="单体" /> -->
    <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" />
    <el-table-column :show-overflow-tooltip="true" prop="plate" label="板型" />
    <el-table-column prop="showQuantity" label="数量" align="center" />
    <!-- <el-table-column :show-overflow-tooltip="true" prop="color" label="颜色" /> -->
    <el-table-column :show-overflow-tooltip="true" :label="`计量单位`" align="center">
      <span>件</span>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="pricingManner" :label="`核算单位`" align="center">
      <template v-slot="scope">
        {{ enclosureSettlementTypeEnum.V[scope.row.pricingManner].UNIT || '-' }}
      </template>
    </el-table-column>
    <!-- <el-table-column :show-overflow-tooltip="true" prop="surfaceArea" :label="`单面积\n(mm²)`" align="center">
      <template v-slot="scope">
        {{ toFixed(scope.row.surfaceArea, DP.COM_AREA__M2) }}
      </template>
    </el-table-column> -->
    <el-table-column :show-overflow-tooltip="true" prop="totalLength" :label="`总长\n(m)`" align="center">
      <template v-slot="scope">
        {{ convertUnits(scope.row.totalLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) }}
      </template>
    </el-table-column>
    <el-table-column :show-overflow-tooltip="true" prop="totalArea" :label="`总面积\n(㎡)`" align="center" width="100px">
      <template v-slot="scope">
        <span>{{ toFixed(scope.row.totalArea, DP.COM_AREA__M2) }}</span>
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
import { convertUnits } from '@/utils/convert/unit'
import { enclosureSettlementTypeEnum } from '@enum-ms/contract'
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
