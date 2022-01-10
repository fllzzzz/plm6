<template>
  <el-table-column v-if="showMeasureUnit" prop="measureUnit" label="计量单位" align="center" width="70px" show-overflow-tooltip>
    <template #default="{ row }">
      <span v-empty-text>{{ row.measureUnit }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showQuantity" :prop="quantityField" :label="quantityLabel" align="right" width="180px" show-overflow-tooltip>
    <template #default="{ row }">
      <template v-if="row.measureUnit">
        <span v-empty-text v-to-fixed="{ val: row[quantityField], dp: row.measurePrecision }" />
        <template v-if="row.maxQuantity < row[quantityField]">
          ->
          <span v-empty-text v-to-fixed="{ val: row.maxQuantity, dp: row.measurePrecision }" class="color-coral" />
        </template>
      </template>
      <span v-else v-empty-text />
    </template>
  </el-table-column>
  <el-table-column v-if="showAccountingUnit" prop="accountingUnit" label="核算单位" align="center" width="70px" show-overflow-tooltip>
    <template #default="{ row }">
      <span v-empty-text>{{ row.accountingUnit }}</span>
    </template>
  </el-table-column>
  <el-table-column v-if="showMete" :prop="meteField" :label="mateLabel" align="right" width="180px" show-overflow-tooltip>
    <template #default="{ row }">
      <span v-empty-text v-to-fixed="{ val: row[meteField], dp: row.accountingPrecision }" />
      <template v-if="row.maxMete < row[meteField]">
        ->
        <span v-empty-text v-to-fixed="{ val: row.maxMete, dp: row.accountingPrecision }" class="color-coral" />
      </template>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { STEEL_ENUM } from '@/settings/config'
import { isBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

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
  showSteelUnit: {
    // 是否显示钢材单位
    type: Boolean,
    default: false
  },
  columns: {
    type: Object
  },
  labelPrefix: {
    // 数量label前缀
    type: String
  },
  quantityField: {
    // 数量字段
    type: String,
    default: 'quantity'
  },
  meteField: {
    // 核算量字段
    type: String,
    default: 'mete'
  },
  showQuantity: {
    type: Boolean,
    default: false
  }
})

const mateLabel = computed(() => {
  let label = ''
  if (props.showUnit && props.showSteelUnit) {
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
      default:
        label = '核算量'
        break
    }
  }
  return (props.labelPrefix || '') + label
})

const quantityLabel = computed(() => {
  let label = ''
  if (props.showUnit && props.showSteelUnit) {
    label = '数量'
  } else {
    switch (props.basicClass) {
      case rawMatClsEnum.STEEL_PLATE.V:
        label = '数量(张)'
        break
      case rawMatClsEnum.SECTION_STEEL.V:
        label = '数量(根)'
        break
      case rawMatClsEnum.STEEL_COIL.V:
        label = '长度(mm)'
        break
      case rawMatClsEnum.MATERIAL.V:
      case rawMatClsEnum.GAS.V:
      default:
        label = '数量'
        break
    }
  }
  return (props.labelPrefix || '') + label
})

// 是否显示单位
const showUnit = computed(() => {
  if (props.basicClass & STEEL_ENUM) {
    return props.showSteelUnit && props.showUnit
  } else {
    return props.showUnit
  }
})

const showMeasureUnit = computed(() => showUnit.value && (isBlank(props.columns) || props.columns.visible('measureUnit')))
const showAccountingUnit = computed(() => showUnit.value && (isBlank(props.columns) || props.columns.visible('accountingUnit')))
const showQuantity = computed(() => props.showQuantity && (isBlank(props.columns) || props.columns.visible(props.quantityField)))
const showMete = computed(() => isBlank(props.columns) || props.columns.visible(props.meteField))
</script>
