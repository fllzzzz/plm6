<template>
  <template v-if="outboundTypeMode">
    <el-table-column v-if="showOutboundUnit" prop="outboundUnit" label="单位" align="center" width="70px">
      <template #default="{ row }">
        <span v-empty-text>{{ row.outboundUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="showCurQuantity" prop="curQuantity" label="数量" align="right" width="100px">
      <template #default="{ row }">
        <span v-empty-text v-to-fixed="row.outboundUnitPrecision">
          {{ row.curOutboundUnitType === measureTypeEnum.MEASURE.V ? row[quantityField] : row[meteField] }}
        </span>
      </template>
    </el-table-column>
  </template>
  <template v-else>
    <el-table-column v-if="showMeasureUnit" prop="measureUnit" label="计量单位" align="center" width="70px">
      <template #default="{ row }">
        <span v-empty-text>{{ row.measureUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="showQuantity" :prop="quantityField" :label="quantityLabel" align="right" width="100px">
      <template #default="{ row }">
        <span v-empty-text v-to-fixed="row.measurePrecision">{{ row[quantityField] }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="showAccountingUnit" prop="accountingUnit" label="核算单位" align="center" width="70px">
      <template #default="{ row }">
        <span v-empty-text>{{ row.accountingUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="showMete" :prop="meteField" :label="mateLabel" align="right" width="100px">
      <template #default="{ row }">
        <span v-empty-text v-to-fixed="row.accountingPrecision">{{ row[meteField] }}</span>
      </template>
    </el-table-column>
  </template>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { measureTypeEnum } from '@/utils/enum/modules/wms'

const props = defineProps({
  basicClass: {
    // 基础分类
    type: Number
  },
  showUnit: {
    // 是否显示单位
    type: Boolean,
    default: true
  },
  outboundTypeMode: {
    // 出库单位 模式（显示出库单位对应的数量及单位）
    type: Boolean,
    default: false
  },
  columns: {
    type: Object
  },
  labelPrefix: { // 数量label前缀
    type: String
  },
  quantityField: { // 数量字段
    type: String,
    default: 'quantity'
  },
  meteField: { // 核算量字段
    type: String,
    default: 'mete'
  }
})

const mateLabel = computed(() => {
  let label = ''
  if (props.showUnit) {
    label = '核算量'
  } else {
    switch (props.basicClass) {
      case rawMatClsEnum.STEEL_PLATE.V:
      case rawMatClsEnum.SECTION_STEEL.V:
      case rawMatClsEnum.STEEL_COIL.V:
        label = '重量(kg)'
        break
      case rawMatClsEnum.MATERIAL.V:
      case rawMatClsEnum.GAS.V:
      default: label = '核算量'
        break
    }
  }
  return (props.labelPrefix || '') + label
})

const quantityLabel = computed(() => {
  let label = ''
  if (props.showUnit) {
    label = '数量'
  } else {
    switch (props.basicClass) {
      case rawMatClsEnum.STEEL_PLATE.V: label = '数量(张)'
        break
      case rawMatClsEnum.SECTION_STEEL.V: label = '数量(根)'
        break
      case rawMatClsEnum.STEEL_COIL.V: label = '长度(mm)'
        break
      case rawMatClsEnum.MATERIAL.V:
      case rawMatClsEnum.GAS.V:
      default: label = '数量'
        break
    }
  }
  return (props.labelPrefix || '') + label
})

const showMeasureUnit = computed(() => props.showUnit && (isBlank(props.columns) || props.columns.visible('measureUnit')))
const showAccountingUnit = computed(() => props.showUnit && (isBlank(props.columns) || props.columns.visible('accountingUnit')))
const showQuantity = computed(() => isBlank(props.columns) || props.columns.visible(props.quantityField))
const showMete = computed(() => isBlank(props.columns) || props.columns.visible(props.meteField))

const showOutboundUnit = computed(() => isBlank(props.columns) || props.columns.visible('outboundUnit'))
const showCurQuantity = computed(() => isBlank(props.columns) || props.columns.visible('curQuantity'))
</script>
