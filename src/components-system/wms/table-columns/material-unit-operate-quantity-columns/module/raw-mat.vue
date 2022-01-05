<template>
  <template v-if="loaded">
    <el-table-column v-if="showMeasureUnit" prop="measureUnit" key="measureUnit" label="计量单位" align="center" width="70px" show-overflow-tooltip>
      <template #default="{ row }">
        <span v-empty-text>{{ row.measureUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column
      v-if="showQuantity"
      key="quantity"
      prop="quantity"
      :label="quantityLabel"
      show-overflow-tooltip
      align="right"
      :min-width="showOperableQuantity ? '150px' : '70px'"
    >
      <template #default="{ row }">
        <template v-if="row.measureUnit">
          <template v-if="showOperableQuantity">
            <span class="operable-number" v-empty-text v-to-fixed="{ val: row[operableQuantityField], dp: row.measurePrecision }" />
            /
          </template>
          <span v-empty-text v-to-fixed="{ val: row[quantityField], dp: row.measurePrecision }" />
        </template>
        <span v-else v-empty-text />
      </template>
    </el-table-column>
    <el-table-column v-if="showAccountingUnit" key="accountingUnit" prop="accountingUnit" label="核算单位" align="center" width="70px" show-overflow-tooltip>
      <template #default="{ row }">
        <span v-empty-text>{{ row.accountingUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="showMete" key="mete" prop="mete" :label="meteLabel" align="right" min-width="150px" show-overflow-tooltip>
      <template #default="{ row }">
        <template v-if="showOperableMete">
          <span class="operable-number" v-empty-text v-to-fixed="{ val: row[operableMeteField], dp: row.accountingPrecision }" />
          /
        </template>
        <span v-empty-text v-to-fixed="{ val: row[meteField], dp: row.accountingPrecision }" />
      </template>
    </el-table-column>
  </template>
</template>
<script setup>
import { defineProps, computed } from 'vue'
import { STEEL_ENUM } from '@/settings/config'
import { isBlank, isNotBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'

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
  singleMeteMode: {
    // 单量模式,只显示单件核算量
    type: Boolean,
    default: false
  },
  meteLabel: {
    // 量-label
    type: String
  },
  labelPrefix: {
    // 列名前缀
    type: String
  },
  quantityField: {
    // 数量字段
    type: String,
    default: 'quantity'
  },
  meteField: {
    // 核算量量字段
    type: String,
    default: 'mete'
  },
  operableQuantityField: {
    // 可操作数量字段
    type: String,
    default: 'operableQuantity'
  },
  operableMeteField: {
    // 可操作核算量量字段
    type: String,
    default: 'operableMete'
  },
  showOperableQuantity: {
    // 显示可操作数量
    type: Boolean,
    default: true
  },
  showOperableMete: {
    type: Boolean,
    default: true
  }
})

// 当前分类基础单位
const { loaded, baseUnit } = useMatBaseUnit()

const unitInfo = computed(() => {
  if (props.basicClass) {
    return baseUnit.value[props.basicClass] || {}
  } else {
    return {}
  }
})

const meteLabel = computed(() => {
  let label = ''
  if (props.meteLabel) {
    label = props.meteLabel
  }
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
    case rawMatClsEnum.SECTION_STEEL.V:
      label = props.singleMeteMode ? `单重(${unitInfo.value.weight.unit})` : `重量(${unitInfo.value.weight.unit})`
      break
    case rawMatClsEnum.STEEL_COIL.V:
      label = `重量(${unitInfo.value.weight.unit})`
      break
    case STEEL_ENUM:
      label = props.singleMeteMode ? `单重(${unitInfo.value.weight.unit})` : `重量(${unitInfo.value.weight.unit})`
      break
    case rawMatClsEnum.MATERIAL.V:
    case rawMatClsEnum.GAS.V:
    default:
      label = props.singleMeteMode ? `单件量` : '核算量'
      break
  }
  if (props.showOperableMete && isNotBlank(props.labelPrefix)) {
    label = props.labelPrefix + ' / ' + label
  }
  return label
})

const quantityLabel = computed(() => {
  let label = ''
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      label = `数量(${unitInfo.value.measure.unit})`
      break
    case rawMatClsEnum.SECTION_STEEL.V:
      label = `数量(${unitInfo.value.measure.unit})`
      break
    case rawMatClsEnum.STEEL_COIL.V:
      label = `长度(${unitInfo.value.measure.unit})`
      break
    case STEEL_ENUM:
      label = `数量(${unitInfo.value.measure.unit})`
      break
    case rawMatClsEnum.MATERIAL.V:
    case rawMatClsEnum.GAS.V:
    default:
      label = '数量'
      break
  }
  if (props.showOperableQuantity && isNotBlank(props.labelPrefix)) {
    label = props.labelPrefix + ' / ' + label
  }
  return label
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
const showQuantity = computed(() => isBlank(props.columns) || props.columns.visible('quantity'))
const showMete = computed(() => isBlank(props.columns) || props.columns.visible('mete'))
</script>

<style lang="scss" scoped>
.operable-number {
  color: green;
}
</style>
