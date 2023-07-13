<template>
  <common-dialog
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="运输价格详情"
    :center="false"
    :close-on-click-modal="false"
  >
    <div class="tip-header">
      <div>
        <span style="font-size:16px;margin-right:3px;">基础价格：{{ toFixed(detailInfo.price, decimalPrecision.mes) }}</span>
        <span :class="detailInfo.priceType === logisticsPriceTypeEnum.WEIGHT.V ? 'blue':'orange'" >{{ logisticsPriceTypeEnum.V[detailInfo.priceType].unit }}</span>
      </div>
      <div style="color:green;font-size:12px;margin-bottom:15px;">*没有填写价格的车型，按照基础价格计算</div>
    </div>
    <common-table
      ref="detailRef"
      border
      :data="detailInfo.list"
      :max-height="maxHeight"
      style="width: 100%"
      class="table-form"
      return-source-data
    >
      <el-table-column label="序号" type="index" align="center" width="50" />
      <el-table-column prop="carModel" label="车型" align="center" />
      <el-table-column prop="priceType" label="计价方式" align="center">
        <template v-slot="scope">
          <span>{{ logisticsPriceTypeEnum.VL[scope.row.priceType] }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="price" label="价格" align="right">
        <template v-slot="scope">
          <span style="margin-right:3px;">{{ toFixed(scope.row.price, decimalPrecision.mes) }}</span>
          <span :class="scope.row.priceType === logisticsPriceTypeEnum.WEIGHT.V ? 'blue':'orange'" v-if="scope.row.price">{{ logisticsPriceTypeEnum.V[scope.row.priceType].unit }}</span>
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { defineProps, defineEmits } from 'vue'

import { toFixed } from '@/utils/data-type'
import { logisticsPriceTypeEnum } from '@enum-ms/mes'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-dialog__header', 'tip-header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false
  },
  visible
)
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
.blue{
  color:#409eff;
}
.orange{
  color:#e6a23c;
}
</style>
