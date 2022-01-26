<template>
  <template v-if="loaded">
    <template v-if="outboundTypeMode">
      <el-table-column
        v-if="showOutboundUnit"
        key="outboundUnit"
        prop="outboundUnit"
        label="单位"
        align="center"
        width="70px"
        :fixed="fixed"
        show-overflow-tooltip
      >
        <template #default="{ row }">
          <span v-empty-text>{{ row.outboundUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="showNumber"
        :key="numberPropField"
        :prop="numberPropField"
        label="数量"
        align="right"
        width="100px"
        :fixed="fixed"
        show-overflow-tooltip
      >
        <template #default="{ row }">
          <span
            v-empty-text
            v-to-fixed="{
              val: row.outboundUnitType === measureTypeEnum.MEASURE.V ? row[quantityField] : row[meteField],
              dp: row.outboundUnitPrecision,
            }"
          />
        </template>
      </el-table-column>
    </template>
    <template v-else>
      <el-table-column
        v-if="showMeasureUnit"
        key="measureUnit"
        prop="measureUnit"
        label="计量单位"
        align="center"
        width="70px"
        :fixed="fixed"
        show-overflow-tooltip
      >
        <template #default="{ row }">
          <span v-empty-text>{{ row.measureUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="showQuantity"
        :prop="quantityField"
        :label="quantityLabel"
        align="right"
        width="100px"
        show-overflow-tooltip
        :fixed="fixed"
      >
        <template #default="{ row }">
          <span v-if="row.measureUnit" v-to-fixed="{ val: row[quantityField], dp: row.measurePrecision }" v-empty-text />
          <span v-else v-empty-text />
        </template>
      </el-table-column>
      <el-table-column
        v-if="showAccountingUnit"
        key="accountingUnit"
        prop="accountingUnit"
        label="核算单位"
        align="center"
        width="70px"
        :fixed="fixed"
        show-overflow-tooltip
      >
        <template #default="{ row }">
          <span v-empty-text>{{ row.accountingUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="showMete"
        :prop="meteField"
        :label="mateLabel"
        align="right"
        width="100px"
        show-overflow-tooltip
        :fixed="fixed"
      >
        <template #default="{ row }">
          <span v-to-fixed="{ val: row[meteField], dp: row.accountingPrecision }" v-empty-text />
        </template>
      </el-table-column>
    </template>
  </template>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { STEEL_ENUM } from '@/settings/config'
import { isBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'

const props = defineProps({
  basicClass: {
    // 基础分类
    type: Number
  },
  fixed: {
    // 固定列
    type: String
  },
  showQuantity: {
    // 是否显示计量量
    type: Boolean,
    default: true
  },
  showMete: {
    // 是否显示核算量
    type: Boolean,
    default: true
  },
  showNumber: {
    // 是否显示数量
    type: Boolean,
    default: true
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
  outboundTypeMode: {
    // 出库单位 模式（显示出库单位对应的数量及单位）
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
  numberPropField: {
    // 默认计量，用于合计时
    type: String,
    default: 'quantity'
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

const mateLabel = computed(() => {
  let label = ''
  if (props.showUnit && props.showSteelUnit) {
    label = '核算量'
  } else {
    switch (props.basicClass) {
      case rawMatClsEnum.STEEL_PLATE.V:
      case rawMatClsEnum.SECTION_STEEL.V:
      case rawMatClsEnum.STEEL_COIL.V:
      case STEEL_ENUM:
        label = `重量(${unitInfo.value.weight.unit})`
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
        label = `数量(${unitInfo.value.measure.unit})`
        break
      case rawMatClsEnum.SECTION_STEEL.V:
        label = `数量(${unitInfo.value.measure.unit})`
        break
      case rawMatClsEnum.STEEL_COIL.V:
        label = `长度(${unitInfo.value.measure.unit})`
        break
      case STEEL_ENUM:
        label = `数量`
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
const showMete = computed(() => props.showMete && (isBlank(props.columns) || props.columns.visible(props.meteField)))

const showOutboundUnit = computed(() => isBlank(props.columns) || props.columns.visible('outboundUnit'))
const showNumber = computed(() => props.showNumber && (isBlank(props.columns) || props.columns.visible(props.numberPropField)))
</script>
